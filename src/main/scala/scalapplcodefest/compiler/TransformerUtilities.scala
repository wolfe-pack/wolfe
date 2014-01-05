package scalapplcodefest.compiler

import scala.tools.nsc.Global

/**
 *
 *
 * @author svivek
 */
object TransformerUtilities {
  def getClassByName(global: Global, fullName: String): global.type#Symbol =
    global.rootMirror.getClassIfDefined(fullName)

  def getClassTypeByName(global: Global, fullName: String): Global#Type =
    getClassByName(global, fullName).tpe


  def getMemberByName(global: Global, fullClassName: String, methodName: String): global.type#Symbol = {
    val clazz = getClassByName(global, fullClassName)
    global.definitions.getMember(clazz, global.newTermName(methodName))
  }

  def getMemberTypeByName(global: Global, fullClassName: String, memberName: String): Global#Type =
    getMemberByName(global, fullClassName, memberName).tpe

}

