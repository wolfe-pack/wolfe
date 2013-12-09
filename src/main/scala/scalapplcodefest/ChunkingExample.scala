package scalapplcodefest

import scala.io.Source
import cc.factorie.optimize.{Perceptron, OnlineTrainer}


/**
 * A first order linear chain for syntactic chunking (shallow parsing).
 *
 * @author Sebastian Riedel
 */
object ChunkingExample {

  import TermImplicits._

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
  val weights = 'weights of Vectors

  //Now create a list of sufficient statistics terms that evaluate to the feature vector of a possible world.

  //summing over all tokens and add a unit vector that is active at an index corresponding to the chunk of the token.
  val bias = vsum(for (i <- 0 ~~ n) yield unit(key('bias, chunk(i))))

  //adding unit vectors corresponding to combination of word and chunk at a token
  val wordChunk = vsum(for (i <- 0 ~~ n) yield unit(key('wordChunk, word(i), chunk(i))))

  //adding a transition feature corresponding to pairs of current and next chunk label
  val trans = vsum(for (i <- 0 ~~ (n - 1)) yield unit(key('trans, chunk(i), chunk(i + 1))))

  //the combination all sufficient statistics vectors
  val feat = bias + wordChunk + trans

  //the model (in log space) is a dot product of sufficient statistics and weights.
  val model = feat dot weights

  def main(args: Array[String]) {

    //this is only needed if we want to print out meaningful feature vectors in low-level code.
    Index.toDebug = Some(key)

    val stream = Util.getStreamFromClassPathOrFile("scalapplcodefest/datasets/conll2000/train.txt")
    val sentences = Util.loadCoNLL(Source.fromInputStream(stream).getLines().take(100), Seq(word, tag, chunk), n)
    val train = sentences.map(_.asTargets(chunk))

    //this is the perceptron loss: maximize over hidden variables in each instance (but condition first on observation)
    //and subtract the model score of the target solution.
    val loss = dsum(SeqTerm(for (i <- train) yield Max.ByMessagePassing(model | i) - (model | i.target)))

    //find a weight vector that minimizes this loss and assign it to the weight variable.
    val learned = state(weights -> GradientBasedMinimizer.minimize(loss, new OnlineTrainer(_, new Perceptron, 10)))

    //a predictor is a mapping from a state to the argument that maximizes the model score conditioned on this state.
    val predict = (s: State) => Max.ByMessagePassing(model | s).argmax.value(learned)

    //a generic evaluation routine that applies the predictor to a set of states and compares the results
    //to target values stored in the state.
    val eval = Evaluator.evaluate(train, predict)


    println(eval)

  }
}


