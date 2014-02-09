package scalapplcodefest.example

import scalapplcodefest._
import scalapplcodefest.TermDSL._
import java.util.zip.GZIPInputStream
import scalapplcodefest.term.{Max, State, Var, Predicate}
import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import scalapplcodefest.term.Var
import scalapplcodefest.term.Predicate

/**
 * A 2nd-order linear-chain CRF for named entity recognition
 *
 * @author rockt
 *         16/12/13
 */
object NERExample extends App {
  val key = new Index()
  Index.toDebug = Some(key) //for debug output

  val labels = set("|O","|B-IUPAC","|I-IUPAC","|B-MODIFIER","|I-MODIFIER") //output labels
  val n = 'n of ints //dynamic sentence length
  val token = 'token of (0 ~~ n |-> strings) //string representation of token at position i
  val label = 'label of (0 ~~ n |-> labels) //label of token at position i

  //val stem = 'stem of (0 ~~ n |-> strings) //TODO: implement feature function abstraction

  val weights = 'weights of vectors //weights that will get learned

  //connections
  val bias = vectors.sum(for (i <- 0 ~~ n) yield unit(key('bias, label(i))))
  val zeroOrder = vectors.sum(for (i <- 0 ~~ n) yield unit(key('zeroOrder, label(i), token(i))))
  val firstOrder = vectors.sum(for (i <- 1 ~~ n) yield unit(key('firstOrder, label(i-1), label(i), token(i))))
  val secondOrder = vectors.sum(for (i <- 2 ~~ n) yield unit(key('secondOrder, label(i-2), label(i-1), label(i), token(i))))

  val features = bias //+ zeroOrder //+ firstOrder //+ secondOrder
  val model = features dot weights

  //load train and test data
  val trainSentences = SCAI.loadSCAI(SCAI.train, Seq(token, label), n)
  val testSentences = SCAI.loadSCAI(SCAI.test, Seq(token, label), n)

  val train = trainSentences.map(_.asTargets(label))
  val test = testSentences.map(_.asTargets(label))

  //FIXME: update to most recent core functions

  /*
  //this is the perceptron loss: maximize over hidden variables in each instance (but condition first on observation)
  //and subtract the model score of the target solution.
  val loss = doubles.sumSeq(for (i <- train) yield Max.ByMessagePassing(model | i) - (model | i.target))

  //find a weight vector that minimizes this loss and assign it to the weight variable.
  val learned = state(weights -> GradientBasedMinimizer.minimize(loss, new OnlineTrainer(_, new Perceptron, 10)))

  //a predictor is a mapping from a state to the argument that maximizes the model score conditioned on this state.
  val predict = (s: State) => Max.ByMessagePassing(model | s).argmax.value(learned)

  //a generic evaluation routine that applies the predictor to a set of states and compares the results to target values stored in the state.
  val eval = Evaluator.evaluate(train, predict)

  println(eval)
  */

  //TODO: evaluate on test set
  //TODO: switch to LBFGS

}

object SCAI {
  val train = io.Source.fromInputStream(util.Util.getStreamFromClassPathOrFile("scalapplcodefest/datasets/scai/train.iob"), "iso-8859-1").getLines().take(100)
  val test = io.Source.fromInputStream(util.Util.getStreamFromClassPathOrFile("scalapplcodefest/datasets/scai/test.iob"), "iso-8859-1").getLines().take(50)

  def groupLines(lines: Iterator[String], delim:String = "") = {
    lines.foldLeft(Seq(Seq.empty[String])) {
      (result, line) => if (line == delim) result :+ Seq.empty else result.init :+ (result.last :+ line )
    }
  }

  def loadSCAI(lines:Iterator[String], predicates: Seq[Predicate[Int, String]], length: Var[Int]) =
    groupLines(lines).map(scaiToState(_,predicates, length))

  def scaiToState(lines: Seq[String], predicates: Seq[Predicate[Int, String]], length: Var[Int]) = {
    import TermDSL._
    val map =
      for {
        (line, i) <- lines.zipWithIndex
        if !line.startsWith("###")
        splits = line.split("\\t+")
        (string, predicate) <- List(splits.head, splits.last) zip predicates //we only need the first and last column
      } yield predicate.atom(i) -> string
    State((map :+ length -> lines.length).toMap)
  }

}