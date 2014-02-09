package scalapplcodefest.macros

import scalapplcodefest.{MaxProduct, UtilOld, Wolfe}
import Wolfe._
import scala.util.Random
import scalapplcodefest.Wolfe.Objective.{Perceptron, Differentiable}
import scalapplcodefest.newExamples.Iris
import scala.io.Source
import scalapplcodefest.util.{Util, Evaluator}
import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import cc.factorie.WeightsSet

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

    //get CoNLL data
    val stream = getStreamFromClassPathOrFile("scalapplcodefest/datasets/conll2000/train.txt")
    val train = loadCoNLL(fromInputStream(stream).getLines(), {case Array(word, tag, chunk) => Token(word, tag, chunk)})

    def S(s: Sentence) = all2(Sentence)(seqs(s.tokens.map(t => all2(Token)(c(Seq(t.word), Seq(t.tag), chunks)))))

    def features(s: Sentence) =
      sum(0 until s.tokens.size)(_ => true)(i => oneHot('bias -> s.tokens(i).chunk)) +
      sum(0 until s.tokens.size)(_ => true)(i => oneHot('obs -> s.tokens(i).chunk -> s.tokens(i).word)) +
      sum(0 until s.tokens.size - 1)(_ => true)(i => oneHot('trans -> s.tokens(i).chunk -> s.tokens(i + 1).chunk))

    def model(w: Vector)(s: Sentence) = features(s) dot w

    def predict(w: Vector)(s: Sentence) = argmax(S(s))(_ => true) {model(w)}



    //the IRIS dataset
    //val train = random.shuffle(loadCoNLL())


  }
}
