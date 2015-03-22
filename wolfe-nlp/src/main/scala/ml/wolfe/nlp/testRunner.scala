package ml.wolfe.nlp

/**
 * Created by Ingolf on 22/03/2015.
 */
object testRunner {
  def main(args: Array[String]): Unit = {

    BasicTokenTest("Hello World")

    val s : Sentence2[BasicTokenTest] = Sentence2.fromString[BasicTokenTest]("Hello World")

    println(s.tokens)
    println(s.tokens.length)
    val d = Document2.fromString[BasicTokenTest,Sentence2[BasicTokenTest]]("Hello World")
    println(d.sentences)
    println(d.sentences.size)


    println("Why not?")
  }
}
