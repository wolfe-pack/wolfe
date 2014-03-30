package ml.wolfe.macros

import ml.wolfe.{MaxProduct, Wolfe}
import Wolfe._
import scala.io.Source
import ml.wolfe.util.Util
import cc.factorie.optimize.{Perceptron, OnlineTrainer}

///**
// * @author Sebastian Riedel
// */
//object TestChunking {
//
//  import OptimizedWolfe._
//
//
//  val chunks = Seq("O", "B-VP", "B-NP", "B-PP", "I-VP", "I-NP", "I-PP", "B-SBAR", "I-SBAR", "B-ADJP", "I-ADJP")
//
//  case class Token(word: String, tag: String, chunk: String)
//  case class Sentence(tokens: Seq[Token])
//
//  def main(args: Array[String]) {
//    import Util._
//    import Source._
//
//
//    //get CoNLL data
//    val stream = getStreamFromClassPathOrFile("ml/wolfe/datasets/conll2000/train.txt")
//    val train = loadCoNLL(fromInputStream(stream).getLines(), {
//      case Array(word, tag, chunk) => Token(word, tag, chunk)
//    }).map(Sentence).take(10)
//
//    println("Chunking ... ")
//
//    def S(s: Sentence) = all(Sentence)(seqs(s.tokens.map(t => all(Token)(c(Seq(t.word), Seq(t.tag), chunks)))))
//
//    //todo: the VectorNumeric is currently needed in the macro, remove this.
//    def features(s: Sentence) =
////      sum(0 until s.tokens.size)(_ => true)(i => oneHot('bias -> s.tokens(i).chunk))(VectorNumeric) +
//      sumOld(0 until s.tokens.size)(_ => true)(i => oneHot('obs -> s.tokens(i).chunk -> s.tokens(i).word))(VectorNumeric) +
//      sumOld(0 until s.tokens.size - 1)(_ => true)(i => oneHot('trans -> s.tokens(i).chunk -> s.tokens(i + 1).chunk))(VectorNumeric)
//
//    @OptimizeByInference(MaxProduct(_,1))
//    def model(w: Vector)(s: Sentence) = features(s) dot w
//
//    @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 5))
//    def loss(weights: Vector) = sumOld(train)(_ => true)(s => max(S(s))(_ => true)(model(weights)) - model(weights)(s))
//
////    val learned = argmin(vectors)(_ => true)(loss)
////
////    def predict(s:Sentence) = argmax(S(s))(_ => true)(model(learned))
////
////    val predicted = train.map(predict)
////
////    println(predicted.mkString("\n"))
////
////    println(Evaluator.evaluate(train.flatMap(_.tokens),predicted.flatMap(_.tokens))(_.chunk))
//
//
//  }
//}
