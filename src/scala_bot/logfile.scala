package scala_bot.logfile

import cats.effect.IO

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala_bot.BotClient
import scala_bot.basics.Game
import scala_bot.logger.{Log, Logger}

val LOG_AUTO_AVAILABLE = true
val LOG_AUTO_AVAILABLE_SEND_MSG = false

private[scala_bot] final case class LogfileContext(
	username: String,
	tableID: Option[Int],
	databaseID: Option[Int],
	game: Option[Game]
)

// Active games use a table ID; completed games are published under the server-assigned database ID.
def logURLDatabase(botName: String, databaseID: Int): String =
	s"https://hanabi.jannisweis.de/logs/${logUsername(botName)}/games/$databaseID.log"

def logURLTable(botName: String, tableID: Int): String =
	s"https://hanabi.jannisweis.de/logs/${logUsername(botName)}/tables/$tableID.log"

private def logUsername(username: String): String =
	URLEncoder.encode(username, UTF_8).replace("+", "%20")

private def logDirTables(username: String): Path =
	Path.of("..", "logs", logUsername(username), "tables")

private def logDirGames(username: String): Path =
	Path.of("..", "logs", logUsername(username), "games")

private def tableLogPath(username: String, tableID: Int): Path =
	logDirTables(username).resolve(s"$tableID.log")

private def gameLogPath(username: String, databaseID: Int): Path =
	logDirGames(username).resolve(s"$databaseID.log")

def ensureActiveLogDirectory(username: String, tableID: Int): IO[Unit] =
	IO.blocking(Logger.setFile(Some(tableLogPath(username, tableID)))).handleErrorWith: error =>
		IO(Log.error(s"Failed to set up file logging: ${error.getMessage}"))

def stopFileLogging: IO[Unit] =
	IO(Logger.setFile(None))

def storeGameLog(username: String, tableID: Int, databaseID: Int): IO[Unit] =
	IO.blocking(storeGameLogSync(username, tableID, databaseID))

def storeGameLogSync(username: String, tableID: Int, databaseID: Int): Unit =
	if databaseID <= 0 then
		println(s"Failed to save log file. DatabaseID: $databaseID")
	else
		val tableLog = tableLogPath(username, tableID)
		if !Files.exists(tableLog) then
			println(s"Log file not found $tableLog")

		val gameLog = gameLogPath(username, databaseID)
		if !Files.exists(gameLog) then
			// Do not replace an existing published log if the server repeats the final init message.
			Files.createDirectories(gameLog.getParent)
			Files.copy(tableLog, gameLog)
			Files.delete(tableLog)

def startGameLog(username: String, tableID: Int, playerNames: Vector[String]): IO[Unit] =
	ensureActiveLogDirectory(username, tableID) *>
	IO:
		Log.info(s"Starting game at ${java.time.Instant.now}")
		Log.info(s"Players: ${playerNames.mkString(", ")}")

extension (client: BotClient)
	def handleInitLogFile(databaseID: Int, reply: String => IO[Unit]): IO[Boolean] =
		client.logfileContext.flatMap:
			case LogfileContext(username, tableID, _, Some(game)) if databaseID > 0 && !game.inProgress =>
				val saveLog = tableID.fold(IO.unit): id =>
					storeGameLog(username, id, databaseID).attempt.flatMap:
						case Right(_) =>
							IO.whenA(LOG_AUTO_AVAILABLE_SEND_MSG):
								reply(s"Saved log file. View it here (database_id=$databaseID): ${logURLDatabase(username, databaseID)}")
						case Left(error) =>
							IO(Log.error(s"Failed to copy log file: ${error.getMessage}")) *>
							IO.whenA(LOG_AUTO_AVAILABLE_SEND_MSG):
								reply("Could not make log file available")

				IO(Log.info(s"Received database_id=$databaseID")) *>
				IO.whenA(LOG_AUTO_AVAILABLE)(saveLog) *>
				// This init confirms the game just ended, so the caller must not initialize a fresh game.
				IO.pure(true)

			case _ => IO.pure(false)

	def handleLogFile(reply: String => IO[Unit]): IO[Unit] =
		client.logfileContext.flatMap:
			case LogfileContext(username, Some(tableID), _, Some(game)) if game.inProgress =>
				reply(s"View the ongoing log here (table_id=$tableID): ${logURLTable(username, tableID)}")

			case LogfileContext(username, Some(tableID), Some(databaseID), _) =>
				storeGameLog(username, tableID, databaseID).attempt.flatMap:
					case Right(_) =>
						reply(s"Saved log file. View it here (database_id=$databaseID): ${logURLDatabase(username, databaseID)}")
					case Left(error) =>
						IO(Log.error(s"Failed to copy log file: ${error.getMessage}")) *>
						reply("Could not make log file available")

			case _ => reply("Could not make log file available")