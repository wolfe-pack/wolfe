package ml.wolfe.util

import com.typesafe.scalalogging._
import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.reflect.macros.whitebox

//.slf4j.Logging

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

/**
 * Logger wrapper class.
 * currently using slf4j-simple. Configuration the logging level:
 * In VM Properties: -Dorg.slf4j.simpleLogger.defaultLogLevel=DEBUG
 *
 */
object LoggerUtil extends LazyLogging {

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

class LogCalls(preHook: String => Unit, postHook: String => Unit = _ => {}) extends StaticAnnotation {
  def macroTransform(annottees: Any*):Any = macro LogCallsMacro.impl

}

object LogCallsMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val withLogging = inputs match {
      case (defDef:DefDef) :: Nil =>
        val msg = Constant(defDef.name.toString)
        val app = c.macroApplication

        val q"$ctr.${_}(${_})" = app
        val q"new ${_}(..$args)" = ctr
//        println(app)
        //http://stackoverflow.com/questions/19379436/cant-access-parents-members-while-dealing-with-macro-annotations/19399119#19399119
        //todo: difficult to get type information in macro annotations.
//        val typed = c.typeCheck(q"${defDef.rhs}")
//        println(typed.tpe)
//        println(c.typeOf[Unit])
//        println(defDef.rhs.tpe =:= c.typeOf[Unit])
        //val unitType = c.typeOf[Unit]
        //defDef.rhs.tpe =:= c.universe.

        //todo: when the type of the passed function is not explicitly defined this seems to fail.
        //todo: need inject type by force in such cases

        //fixme: check for return type
        val tmp = TermName(c.freshName("tmp"))
        //defDef.tpe //return type of the def? might also be the structured type!?
        //alternative: check type of last expression

        val newRhs = args match {
          case pre :: Nil =>
            val passPreMsg = q"$pre($msg)"
            Block(List(passPreMsg), defDef.rhs)
          case pre :: post :: Nil =>
            val passPreMsg = q"$pre($msg)"
            val passPostMsg = q"$post($msg)"
            defDef.rhs match {
              //fixme: val tmp = expr is Unit!
              case Block(statements, expr) => Block(passPreMsg :: (statements ++ List(q"val $tmp = $expr", q"$passPostMsg")), q"$tmp")
              case expr => Block(List(passPreMsg, q"val $tmp = $expr", q"$passPostMsg"), q"$tmp")
            }
          case _ => c.abort(c.enclosingPosition, "LogCalls can only handle a pre and post hook")
        }

        treeCopy.DefDef(defDef,defDef.mods,defDef.name,defDef.tparams,defDef.vparamss,defDef.tpt,newRhs)
      case _ =>
        c.abort(c.enclosingPosition, "LogCalls can only annotate methods")
    }
    c.Expr[Any](withLogging)
  }
}

