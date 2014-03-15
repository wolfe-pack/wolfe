package ml.wolfe.macros

import scala.reflect.api.Universe

/**
 * @author Sebastian Riedel
 */
trait WolfeSymbols extends HasUniverse {

  import universe._

  lazy val wolfe  = rootMirror.staticModule("ml.wolfe.Wolfe")
  lazy val all    = wolfe.typeSignature.member(newTermName("all2"))
  lazy val unwrap = wolfe.typeSignature.member(newTermName("unwrap2"))
  //println("Blah: " + wolfe)
}

object WolfeSymbols {
  def apply(u: Universe): WolfeSymbols {type U = u.type} = new WolfeSymbols {
    val universe: u.type = u
    type U = u.type
  }
}