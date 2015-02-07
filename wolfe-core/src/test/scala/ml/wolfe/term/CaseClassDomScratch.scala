package ml.wolfe.term

/**
 * @author riedel
 */
object CaseClassDomScratch {

  import ml.wolfe.term.TermImplicits._
  import scala.language.implicitConversions
  case class World(rain: Boolean, sprinkler: Boolean, probRain: Double)
  object World {
    def dom(rainDom:TypedDom[Boolean], sprinkler:TypedDom[Boolean], probRain:TypedDom[Double]): TypedDom[World] = ???
  }

  def main(args: Array[String]) {
    val world = World(true,true,1.0)

    @domain[World](bools, bools, doubles) object worlds
//    val impWorlds = worlds
//    implicit val strings = discrete("A","B")
//
//    import worlds.conversion._
////    import strings.conversion._
//
//    val x = worlds.variable("x")
//    def prob(y:worlds.Term) = y.probRain * I(y.rain)
//    val term = prob(x)
//    println(term.eval(World(true,true,10.0)))
//
//    val cast:worlds.Term = World(true,true,10.0)
//    //println(cast)
//    //println(rain)
//
//    val imp = implicitly[DiscreteDom[String]]
//    println(imp)
//    val text:Term[TypedDom[String]] = "A"
//
//    //println(text)
//
//    val test = worlds.const(World(true,true,1.0))
//    println(worlds.one)
//    println(worlds.zero)

  }



//  println(Tokens.x)
//  val test:Tokens.Value = Token(true,true,1.0)

}

