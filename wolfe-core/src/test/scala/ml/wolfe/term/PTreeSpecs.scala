package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author rockt
 */
class PTreeSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  implicit class MarginalsMap[T](map: Map[T, Double]) {
    def sum = map.values.sum

    def expNormalize = {
      val normalizer = math.log(map.mapValues(math.exp).valuesIterator.sum)
      map map (p => p.copy(_2 = math.exp(p._2 - normalizer)))
    }
  }

  def toParse(graph: Set[(Int, Int)], slen: Int): IndexedSeq[IndexedSeq[Boolean]] = {
    for (mod <- 0 until slen) yield for (head <- 0 until slen) yield graph(head -> mod)
  }

  def treesAtRoot(root: Int, t2: Int, t3: Int) =
    Seq(
      Set(root -> t2, t2 -> t3),
      Set(root -> t2, root -> t3),
      Set(root -> t3, t3 -> t2)
    )

  //all correct trees?
  val trees = (treesAtRoot(0, 1, 2) ++ treesAtRoot(1, 0, 2) ++ treesAtRoot(2, 1, 0)) map (toParse(_, 3))

  //examples of wrong trees
  val withCycles = Seq(
    Set(1 -> 2, 2 -> 1),
    Set(0 -> 2, 2 -> 0),
    Set(0 -> 0, 1 -> 2),
    Set(1 -> 1, 2 -> 2),
    Set(0 -> 0, 2 -> 2)
  ) map (toParse(_, 3))

  val crossing = Seq(
    Set(0 -> 2, 2 -> 1, 1 -> 3)
  ) map (toParse(_, 4))

  val wrongLength = Seq(
    Set(0 -> 1, 1 -> 2),
    Set(0 -> 1)
  ) map (toParse(_, 4))

  val nonTrees = withCycles ++ wrongLength ++ crossing

  "A PTree term over a variable length parse" should {

    val minLength = 3
    val maxLength = 5
    val Parses = Seqs(Seqs(Bools, minLength, maxLength), minLength, maxLength)
    val slen = Ints.Var
    val parse = Parses.Var
    val constraint = PTree(parse, slen)

    "evaluate to 0 if the input is a projective tree" in {

      for (tree <- trees) {
        constraint.eval(parse := tree, slen := 3) should be(0.0)
      }
    }

    "evaluate to negative infinity if the tree has cycles" in {

      for (nonTree <- withCycles) {
        constraint.eval(parse := nonTree, slen := 3) should be(Double.NegativeInfinity)
      }
    }

    "evaluate to negative infinity if the tree has crossing edges" in {

      for (nonTree <- crossing) {
        constraint.eval(parse := nonTree, slen := 4) should be(Double.NegativeInfinity)
      }

    }

    "evaluate to negative infinity if the tree has the wrong length" in {

      for (nonTree <- wrongLength) {
        constraint.eval(parse := nonTree, slen := 4) should be(Double.NegativeInfinity)
      }

    }

  }

  "A PTree term over a fixed length parse" should {

    val length = 3
    val Parses = Seqs(Seqs(Bools, length), length)
    val slen = Ints.Var
    val parse = Parses.Var
    val constraint = PTree(parse, slen)


    "evaluate marginals in a brute-force manner for a single structured parse variable" in {
      val input = Settings(Setting.disc(length))
      val msg = Msgs(Parses.createZeroMsg())
      val marginalizer = new ExhaustiveSearchMarginalizer(constraint, Seq(parse), Seq(slen), input, msg, true)
      marginalizer.marginals()
      val result = Parses.toMarginals(marginalizer.outputMsgs(0))
      //the exponentiated log-marginals should correspond to counts
      //there are 9 legal parses
      //each non-self-loop appears in 3 such parses (and is false in 6)
      for (m <- 0 until length; h <- 0 until length) {
        if (h == m) {
          result(m)(h)(true) should be(Double.NegativeInfinity)
          result(m)(h)(false) should be(math.log(9) +- eps)
        } else {
          result(m)(h)(true) should be(math.log(3) +- eps)
          result(m)(h)(false) should be(math.log(6) +- eps)
        }

      }
    }


  }

}
