package ml.wolfe.term

/**
 * @author riedel
 */
object CaseClassDomScratch {

  import ml.wolfe.term.TermImplicits._
  case class World(rain: Boolean, sprinkler: Boolean, probRain: Double)

  def main(args: Array[String]) {

    @domain[World](bools, bools, doubles) object Worlds

    val x = Worlds.variable("x")
    def prob(y:Worlds.Term) = y.probRain * I(y.rain)
    val term = prob(x)
    println(term.eval(World(true,true,10.0)))

    val test = Worlds.const(World(true,true,1.0))
    println(Worlds.one)
    println(Worlds.zero)

  }



//  println(Tokens.x)
//  val test:Tokens.Value = Token(true,true,1.0)

}

