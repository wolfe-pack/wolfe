package scalapplcodefest.macros

import scalapplcodefest.Wolfe
import Wolfe._
import scalapplcodefest.Wolfe.Objective.Perceptron
import scala.io.Source
import scalapplcodefest.util.Util
import cc.factorie.optimize.{Perceptron, OnlineTrainer}

/**
 * @author Sebastian Riedel
 */
object TestChunking {

  import OptimizedWolfe._


  val chunks = Seq("O", "B-VP", "B-NP", "B-PP", "I-VP", "I-NP", "I-PP", "B-SBAR", "I-SBAR", "B-ADJP", "I-ADJP")

  case class Token(word: String, tag: String, chunk: String)
  case class Sentence(tokens: Seq[Token])

  def main(args: Array[String]) {
    import Util._
    import Source._

    println("Chunking ..")

    //get CoNLL data
    val stream = getStreamFromClassPathOrFile("scalapplcodefest/datasets/conll2000/train.txt")
    val train = loadCoNLL(fromInputStream(stream).getLines().take(100), {
      case Array(word, tag, chunk) => Token(word, tag, chunk)
    }).map(Sentence)

    def S(s: Sentence) = all2(Sentence)(seqs(s.tokens.map(t => all2(Token)(c(Seq(t.word), Seq(t.tag), chunks)))))

    //todo: the VectorNumeric is currently needed in the macro, remove this.
    def features(s: Sentence) =
      sum(0 until s.tokens.size)(_ => true)(i => oneHot('bias -> s.tokens(i).chunk))(VectorNumeric) +
      sum(0 until s.tokens.size)(_ => true)(i => oneHot('obs -> s.tokens(i).chunk -> s.tokens(i).word))(VectorNumeric) +
      sum(0 until s.tokens.size - 1)(_ => true)(i => oneHot('trans -> s.tokens(i).chunk -> s.tokens(i + 1).chunk))(VectorNumeric)

    def model(w: Vector)(s: Sentence) = features(s) dot w

    //    def predict(w: Vector)(s: Sentence) = argmax(S(s))(_ => true) {model(w)}

    @MinByDescent(new OnlineTrainer(_, new Perceptron, 4))
    def loss(weights: Vector) = sum(train.take(1))(_ => true)(s => max(S(s))(_ => true)(model(weights)) - model(weights)(s))

    val learned = argmin(vectors)(_ => true)(loss)

    def predict(s:Sentence) = argmax(S(s))(_ => true)(model(learned))

    //use a generic def wildcard[T](implicit provider: WildcardProvider[T]) = provider.wildcard
    def observed(s:Sentence) = s.copy(tokens = s.tokens.map(_.copy(chunk = "")))

  }
}
