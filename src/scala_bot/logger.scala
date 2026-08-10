package scala_bot.logger

import scala.Console._
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}

object LogLevel:
	inline val Off = 0
	inline val Error = 1
	inline val Warn  = 2
	inline val Info  = 3

object Logger:
	@volatile var runtimeLevel: Int = LogLevel.Info
	@volatile private var logFile: Option[Path] = None

	def setLevel(level: Int): Unit =
		runtimeLevel = level

	def level = runtimeLevel

	def setFile(path: Option[Path]): Unit = synchronized:
		if logFile != path then
			path.foreach: file =>
				val parent = file.getParent
				if parent != null then
					Files.createDirectories(parent)
					()
				// A new table log starts empty; revisiting the same path preserves it.
				Files.writeString(file, "", UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
				()
			logFile = path

	def writeToFile(message: String): Unit = synchronized:
		logFile.foreach: file =>
			try
				Files.writeString(file, s"$message${System.lineSeparator}", UTF_8, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
				()
			catch
				case error: Exception =>
					System.err.println(s"Failed to write log file: ${error.getMessage}")
					logFile = None

object Log:
	inline val compileTimePriority = 0		// Change this value to 3 to enable full debug logs

	inline def log(inline level: Int, inline msg: => String, inline colour: String = WHITE): Unit =
		inline if level <= compileTimePriority then
			if level <= Logger.runtimeLevel then
				val message = msg
				println(s"$colour$message$RESET")
				Logger.writeToFile(message)
		else
			()

	inline def error(inline msg: => String): Unit = log(LogLevel.Error, msg, MAGENTA)
	inline def warn(inline msg: => String): Unit = log(LogLevel.Warn, msg, CYAN)
	inline def info(inline msg: => String): Unit = log(LogLevel.Info, msg)
	inline def highlight(inline colour: String, inline msg: => String): Unit = log(LogLevel.Info, msg, colour)
