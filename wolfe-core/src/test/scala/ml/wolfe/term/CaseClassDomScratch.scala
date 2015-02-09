package ml.wolfe.term

/**
 * @author riedel
 */
object CaseClassDomScratch {

  import ml.wolfe.term.TermImplicits._
  import scala.language.implicitConversions

  case class World(rain: Boolean, prob: Double)


  def main(args: Array[String]) {
    //@domain case class Universe(worlds:IndexedSeq[Double])

//    @domain2 case class World(rain: Boolean, sprinkler: Boolean, probRain: Double)
//
//    val world = World(true,true,1.0)
//    val worlds = World.Dom(bools,bools,doubles)
//    val worlds2 = World.Dom // use implicits
//
//    println(worlds)
//    println(worlds.lengths)
    //val m:worlds.Marginals = ???

//    @domain[World](bools, bools, doubles) object worlds
//
//    println(worlds)
//    println(_Blub.dom)
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

