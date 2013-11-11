package scalapplcodefest


/**
 * @author Sebastian Riedel
 */
object ChunkingExample {

  import TermImplicits._

  def main(args: Array[String]) {
    val Chunks = Set("O", "B-VP", "B-NP", "B-PP", "I-VP", "I-NP", "I-PP")
    val SentenceLengths = Constant(Range(0, 1000).toSet)
    val n = Var('n, SentenceLengths)
    val triple = Var('pair, SentenceLengths x SentenceLengths x SentenceLengths)
    val Tokens = RangeSet(0, n)
    val chunk = Predicate('chunk, RangeSet(0, n), Chunks)
    val next = Predicate('neighbor, Constant(Set(0 -> 1)), Bools)
    val next2 = Predicate('next2, Tokens x Tokens, Bools)
    val atom = chunk.atom(0)
    val atom2 = chunk(n)
    val atom3 = next.atom(0, 0)
    val atom4 = next(0, n)
    val test = n + 2
    val test2 = for (i <- Tokens) yield chunk(i + 1)
    val test3 = Tokens.flatMap(i => Tokens.map( j => chunk(i)))
    val test4 = for (i <- Tokens; j <- Tokens) yield next(i,j)
    val test5 = for (i <- Tokens; j <- Tokens; k <- Tokens) yield i + k + j
    val plus1 = for (x <- Ints) yield x + 1
    val plus2 = for (x <- Ints; y <- Ints) yield x + y
    val plus3 = for (x <- Ints; y <- Ints; z <- Ints) yield x + y + z

    val eval = plus1.eval(State.empty)
    val Curried2(uncurried) = plus2


    println(uncurried.eval(State.empty).get(2->30))
    println(eval.get(3))
    println(plus2.eval(State.empty).get(2)(3))
    println(plus3.eval(State.empty).get(2)(3)(4))




    val stream = Util.getStreamFromClassPathOrFile("scalapplcodefest/datasets/conll2000/train.txt")

  }
}
