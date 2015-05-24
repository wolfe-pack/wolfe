package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author rockt
 */
class PTreeSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  def toParse(graph:Set[(Int,Int)], slen:Int):IndexedSeq[IndexedSeq[Boolean]] = {
    for (mod <- 0 until slen) yield for (head <- 0 until slen) yield graph(head -> mod)
  }

  def treesAtRoot(root:Int, t2:Int, t3:Int) =
    Seq(
      Set(root -> t2, t2 -> t3),
      Set(root -> t2, root -> t3),
      Set(root -> t3, t3 -> t2)
    )

  //all correct trees?
  val trees = (treesAtRoot(0,1,2) ++ treesAtRoot(1,0,2) ++ treesAtRoot(2,1,0)) map (toParse(_,3))

  //examples of wrong trees
  val withCycles = Seq(
    Set(1 -> 2, 2 -> 1),
    Set(0 -> 2, 2 -> 0),
    Set(0 -> 0, 1 -> 2)
  ) map (toParse(_,3))

  val crossing = Seq(
    Set(0 -> 2, 2 -> 1, 1 -> 3)
  ) map (toParse(_,4))

  val wrongLength = Seq(
    Set(0 -> 1, 1 -> 2),
    Set(0 -> 1)
  ) map (toParse(_,4))

  val nonTrees = withCycles ++ wrongLength ++ crossing

  "A PTree term" should {

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
        constraint.eval(parse := nonTree, slen := 4) should be (Double.NegativeInfinity)
      }

    }


  }
}
