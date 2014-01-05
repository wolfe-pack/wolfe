package scalapplcodefest

import scala.tools.nsc.Global
import scala.annotation.StaticAnnotation

/**
 * User: rockt
 * Date: 1/2/14
 * Time: 3:36 PM
 */
package object compiler {
  def sameAnnotation[T <: Global#Tree](a1: Global#AnnotationInfo, a2: StaticAnnotation): Boolean =
    a1.toString.startsWith(a2.getClass.getName.replace('$','.'))

  def existsAnnotation[T <: Global#Tree](fun: Global#DefDef, annotation: StaticAnnotation): Boolean =
    fun.symbol.annotations.exists(a => sameAnnotation(a, annotation))

  def dirPathOfClass(className: String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val root = path.dropRight(resource.length)
    root
  }
}
