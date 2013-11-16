package scalapplcodefest

import scalapplcodefest.Math.UnitVec
import scala.io.Source


/**
 * @author Sebastian Riedel
 */
object ChunkingExample {

  import TermImplicits._

  def toState(lines: Seq[String], predicates: Seq[Predicate[Int, String]], length: Var[Int]) = {
    val map = for ((line, i) <- lines.zipWithIndex;
                   (s, pred) <- line.split("\\s+") zip predicates) yield pred.atom(i) -> s
    State((map :+ length -> lines.length).toMap)
  }


  def main(args: Array[String]) {
    val Chunks = Set("O", "B-VP", "B-NP", "B-PP", "I-VP", "I-NP", "I-PP", "B-SBAR", "I-SBAR", "B-ADJP", "I-ADJP")
    val SentenceLengths = RangeSet(0, 1000)
    val n = Var('n, SentenceLengths)
    val Tokens = RangeSet(0, n)
    val chunk = Predicate('chunk, RangeSet(0, n), Chunks)
    val word = Predicate('word, RangeSet(0, n), Strings)
    val tag = Predicate('tag, RangeSet(0, n), Strings)
    val tag2 = 'tag of (0 ~~ 2 |-> Strings)
    val w2 = 'w2 of Vecs

    val triple = Var('pair, SentenceLengths x SentenceLengths x SentenceLengths)
    val next = Predicate('neighbor, Constant(Set(0 -> 1)), Bools)
    val next2 = Predicate('next2, Tokens x Tokens, Bools)
    val atom = chunk.atom(0)
    val atom2 = chunk(n)
    val atom3 = next.atom(0, 0)
    val atom4 = next(0, n)
    val test = n + 2
    val test2 = for (i <- Tokens) yield chunk(i + 1)
    val test3 = Tokens.flatMap(i => Tokens.map(j => chunk(i)))
    val test4 = for (i <- Tokens; j <- Tokens) yield next(i, j)
    val test5 = for (i <- Tokens; j <- Tokens; k <- Tokens) yield i + k + j
    val Dom = Constant(Ints)
    val plus1 = for (x <- Dom) yield x + 1
    val plus2 = for (x <- Dom; y <- Dom) yield x + y
    val plus3 = for (x <- Dom; y <- Dom; z <- Dom) yield x + y + z

    val t = plus2(1)(2)


    val feat2 = Quantified.VecSum(for (i <- Tokens; j <- Tokens) yield e_(i))

    val weights = Var('weights, Vecs)

    val model = feat2 dot weights

    //println(feat2.eval(n -> 2))

    val eval = plus1.eval(State.empty)
    val Curried2(uncurried) = plus2

    println(plus3.variables)
    println(test.variables)
    println((test2 | n -> 2).variables)

    println(uncurried.eval(State.empty).get(2 -> 30))
    println(eval.get(3))
    println(plus2.eval(State.empty).get(2)(3))
    println(plus3.eval(State.empty).get(2)(3)(4))

    val allBinaryOperators = AllFunctions(CartesianProduct2(Bools, Bools), Bools)
    for (op <- allBinaryOperators) {
      println("---")
      for (a1 <- Bools; a2 <- Bools) println(op(a1 -> a2))
    }

    val index = ConstantFun(new Index()) //can we convert this implicitly?

    val app = index(1, 2, 3, 4)
    val f = for (i <- RangeSet(0, n); j <- RangeSet(0, n)) yield i + j

    println(ImageSeqCurried2(f).eval(n -> 2).map(_.mkString(",")))

    val stream = Util.getStreamFromClassPathOrFile("scalapplcodefest/datasets/conll2000/train.txt")
    val sentences = Util.groupLines(Source.fromInputStream(stream).getLines().take(100))
    val states = sentences.map(toState(_, Seq(word, tag, chunk), n))

    println(states.head.toPrettyString)
    val hidden = states.head.asTargets(AllGroundAtoms(chunk))
    println(hidden.toPrettyString)

    implicit val $ = new Index()
    val feat = vsum(for (i <- 0 ~~ n) yield e_($(1, chunk(i))))
    val first = states.head

    val vec = feat.eval(first).get
    val w = Var('w, Vecs)


    println(vec.toMappedString)

    val m = (feat dot w) + 1.0


  }
}


