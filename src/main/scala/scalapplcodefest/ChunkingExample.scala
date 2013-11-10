package scalapplcodefest


/**
 * @author Sebastian Riedel
 */
object ChunkingExample {

  import TermImplicits._

  def main(args: Array[String]) {
    val Chunks = Set("O", "B-VP", "B-NP", "B-PP", "I-VP", "I-NP", "I-PP")
    val SentenceLengths = Range(0, 1000).toSet
    val n = Var('n,SentenceLengths)
    val Tokens = RangeSet(0,n)
    val chunk = Predicate('chunk, Tokens, Chunks)

    val stream = Util.getStreamFromClassPathOrFile("scalapplcodefest/datasets/conll2000/train.txt")

  }
}
