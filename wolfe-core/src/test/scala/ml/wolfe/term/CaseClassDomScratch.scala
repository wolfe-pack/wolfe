package ml.wolfe.term

/**
 * @author riedel
 */
object CaseClassDomScratch {

  import ml.wolfe.term.TermImplicits._
  case class Token(word: Boolean, index: Boolean, value: Double)

  def main(args: Array[String]) {

    @domain[Token](bools, bools, doubles) object Tokens


    val test = Tokens.const(Token(true,true,1.0))
    println(Tokens.one)
    println(Tokens.zero)

  }



//  println(Tokens.x)
//  val test:Tokens.Value = Token(true,true,1.0)

}

