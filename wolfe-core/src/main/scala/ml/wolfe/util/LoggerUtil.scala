package ml.wolfe.util

import com.typesafe.scalalogging.slf4j.{Logging}

/**
 * Logger wrapper class.
 * currently using slf4j-simple. Configuration the logging level:
 * In VM Properties: -Dorg.slf4j.simpleLogger.defaultLogLevel=DEBUG
 *
 */
object LoggerUtil extends Logging {

  def getLogger = logger.underlying

  val seenBefore:scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
  def once(function:String=>Unit, msg:String, details:String = "") {
    if(! seenBefore(msg)) {
      seenBefore += msg
      function(msg)
      if(details.nonEmpty) function(">> " + details)
    }
  }

  def info(msg: String) = {
    logger.info(msg)
  }
  def warn(msg: String) = {
    logger.warn(msg)
  }
  def debug(msg: String) = {
    logger.debug(msg)
  }
  def error(msg: String) = {
    logger.error(msg)
  }
  def trace(msg: String) = {
    logger.trace(msg)
  }

  def info(msg: String, t: Throwable) = {
    logger.info(msg, t)
  }

  def warn(msg: String, t: Throwable) = {
    logger.warn(msg, t)
  }

  def debug(msg: String, t: Throwable) = {
    logger.debug(msg, t)
  }
  def error(msg: String, t: Throwable) = {
    logger.error(msg, t)
  }
  def trace(msg: String, t: Throwable) = {
    logger.trace(msg, t)
  }
}
