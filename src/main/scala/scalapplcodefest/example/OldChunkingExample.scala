package scalapplcodefest.example

import scala.io.Source
import cc.factorie.optimize.{AdaGrad, AveragedPerceptron, Perceptron, OnlineTrainer}
import scalapplcodefest.value.Vectors
import scalapplcodefest.term.{State, Max, SeqTerm}
import scalapplcodefest._
import scalapplcodefest.TermDSL._
import scalapplcodefest.term.SeqTerm
import scala.Some
import scalapplcodefest.term.SeqTerm
import scala.Some


/**
 * A first order linear chain for syntactic chunking (shallow parsing).
 *
 * @author Sebastian Riedel
 */
object OldChunkingExample {

  import TermDSL._

  // An map from value sequences (such as word,chunk pairs) to integer indices into a vector space.
  val key = new Index()

  //All possible chunk types
  val chunks = set("O", "B-VP", "B-NP", "B-PP", "I-VP", "I-NP", "I-PP", "B-SBAR", "I-SBAR", "B-ADJP", "I-ADJP")

  //a variable corresponding to sentence length.
  val n = 'n of ints

  //a predicate that maps tokens (integers) in a sentence to their words. Note that the domain of this predicate is dynamic.
  val word = 'word of (0 ~~ n |-> strings)

  //a predicate mapping tokens to their Part-of-Speech.
  val tag = 'tag of (0 ~~ n |-> strings)

  //a predicate mapping tokens to their chunk labels. We use the Chunk type as range (and not Strings) as we need to search over it.
  val chunk = 'chunk of (0 ~~ n |-> chunks)

  //a variable corresponding to the weight vector of the model. Learning will find an assignment to this variable.
  val weights = 'weights of vectors

  //Now create a list of sufficient statistics terms that evaluate to the feature vector of a possible world.

  //summing over all tokens and add a unit vector that is active at an index corresponding to the chunk of the token.
  val bias = vectors.sum(for (i <- 0 ~~ n) yield unit(key('bias, chunk(i))))

  //adding unit vectors corresponding to combination of word and chunk at a token
  val wordChunk = vectors.sum(for (i <- 0 ~~ n) yield unit(key('wordChunk, word(i), chunk(i))))

  //adding a transition feature corresponding to pairs of current and next chunk label
  val trans = vectors.sum(for (i <- 0 ~~ (n - 1)) yield unit(key('trans, chunk(i), chunk(i + 1))))

  //the combination all sufficient statistics vectors
  val feat = bias + wordChunk + trans

  //the model (in log space) is a dot product of sufficient statistics and weights.
  val model = feat dot weights

  def main(args: Array[String]) {

    //this is only needed if we want to print out meaningful feature vectors in low-level code.
    Index.toDebug = Some(key)

    //load a stream of lines from the resource.
    val stream = util.Util.getStreamFromClassPathOrFile("scalapplcodefest/datasets/conll2000/train.txt")

    //turn the CoNLL format sentences into states. Map the second column to the word function, the second to the
    //tag function, the third to the chunk function. The sentence length is stored in an assignment for `n`.
    val sentences = UtilOld.loadCoNLL(Source.fromInputStream(stream).getLines().take(100), Seq(word, tag, chunk), n)

    //convert assignments to the chunk predicate to assignments that indicate that chunk variable is the target of prediction.
    val train = sentences.map(_.asTargets(chunk))

    //this is the perceptron loss/reward: maximize over hidden variables in each instance (but condition first on observation)
    //and subtract the model score of the target solution.
    val obj = doubles.sumSeq(for (i <- train) yield max(lam(chunk, model | i)).byMessagePassing() - (model | i.target))

    //find a weight vector that minimizes this loss and assign it to the weight variable.
    val learned = argState(min(lam(weights, obj)).byTrainer(new OnlineTrainer(_, new AdaGrad(), 6))).value()

    //a predictor is a mapping from a state to the argument that maximizes the model score conditioned on this state.
    val predict = (s: State) => argState(max(lam(chunk, model | s)).byMessagePassing()).value(learned)

    //a generic evaluation routine that applies the predictor to a set of states and compares the results
    //to target values stored in the state.
    val eval = Evaluator.evaluate(train, predict)

    println(eval)

  }
}


