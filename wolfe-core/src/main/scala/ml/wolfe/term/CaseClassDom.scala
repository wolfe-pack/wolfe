package ml.wolfe.term

import scala.language.experimental.macros

/**
 * @author riedel
 */
object CaseClassDom {

//  def all[C,D1 <: Dom](dom1:D1):TypedDom[C] = macro allImpl[C,D1]
//
//  def allImpl[C, D1 <: Dom](c:whitebox.Context)(dom1:c.Expr[D1]):c.Expr[TypedDom[C]] = ???



  trait ProductDomain[C]


  def main(args: Array[String]) {

    object Tokens extends ProductDomain[(String,String,Int,String)]


  }
}


//trait Domain[T]
//
//class dom(implicit dom1:Any) extends StaticAnnotation {
//  def macroTransform(annottees: Any*) = macro ???
//}
