package ml.wolfe.macros

import scala.reflect.api.Universe

/**
 * @author Sebastian Riedel
 */
trait WolfeSymbols extends HasUniverse {

  import universe._

  lazy val wolfe   = rootMirror.staticModule("ml.wolfe.Wolfe")
  lazy val wolfeType = wolfe.typeSignature
  lazy val all     = wolfeType.member(newTermName("all"))
  lazy val unwrap2 = wolfeType.member(newTermName("unwrap2"))
  lazy val unwraps = Range(2, 6).map(i => wolfeType.member(newTermName("unwrap" + i))).toSet
  lazy val crosses = Range(2, 4).map(i => wolfeType.member(newTermName("Cross" + i))).toSet

  //println("Blah: " + wolfe)
}

object WolfeSymbols {
  def apply(u: Universe): WolfeSymbols {type U = u.type} = new WolfeSymbols {
    val universe: u.type = u
    type U = u.type
  }
}