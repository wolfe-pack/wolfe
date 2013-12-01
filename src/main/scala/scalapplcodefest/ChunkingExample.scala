package scalapplcodefest

import scala.io.Source


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
  val Chunks = SetValue("O", "B-VP", "B-NP", "B-PP", "I-VP", "I-NP", "I-PP", "B-SBAR", "I-SBAR", "B-ADJP", "I-ADJP")

  //a variable corresponding to sentence length.
  val n = 'n of Ints

  //a predicate that maps tokens (integers) in a sentence to their words. Note that the domain of this predicate is dynamic.
  val word = 'word of (0 ~~ n |-> Strings)

  //a predicate mapping tokens to their Part-of-Speech.
  val tag = 'tag of (0 ~~ n |-> Strings)

  //a predicate mapping tokens to their chunk labels. We use the Chunk type as range (and not Strings) as we need to search over it.
  val chunk = 'chunk of (0 ~~ n |-> Chunks)

  //a variable corresponding to the weight vector of the model. Learning will find an assignment to this variable.
  val weights = 'weights of Vectors

  //Now create a list of sufficient statistics terms that evaluate to the feature vector of a possible world.

  //summing over all tokens and add a unit vector that is active at an index corresponding to the chunk of the token.
  val bias = vsum(for (i <- 0 ~~ n) yield unit(key('bias,chunk(i))))

  //adding unit vectors corresponding to combination of word and chunk at a token
  val wordChunk = vsum(for (i <- 0 ~~ n) yield unit(key('wordChunk,word(i),chunk(i))))

  //adding a transition feature corresponding to pairs of current and next chunk label
  val trans = vsum(for (i <- 0 ~~ (n-1)) yield unit(key('trans, chunk(i),chunk(i+1))))

  //the combination all sufficient statistics vectors
  val feat = bias + wordChunk + trans

  //the model (in log space) is a dot product of sufficient statistics and weights.
  val model =  feat dot weights

  def main(args: Array[String]) {

    val stream = Util.getStreamFromClassPathOrFile("scalapplcodefest/datasets/conll2000/train.txt")
    val sentences = Util.loadCoNLL(Source.fromInputStream(stream).getLines().take(100),Seq(word, tag, chunk), n)
    val train = sentences.map(_.asTargets(chunk))

    //println(train.head.toPrettyString)

    val learnedWeights = Trainer.train(model, train, 10, Inference.maxProductArgmax(1,chunk.allAtoms))
    val predictor = (s:State) => Inference.maxProductArgmax(1,chunk.allAtoms)(model | s | weights -> learnedWeights ).state()
    val evaluation = Evaluator.evaluate(train,predictor)

    //println(key.vectorToString(learnedWeights))
    println(evaluation)

  }
}


