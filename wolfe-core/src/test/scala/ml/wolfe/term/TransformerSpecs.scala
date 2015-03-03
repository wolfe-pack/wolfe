package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class TransformerSpecs extends WolfeSpec {

  import TermImplicits._
  import Transformer._

  "A depth first transformer" should {
    "transform a term tree in a depth first fashion" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val term = x * 2.0 + x
      val expected = y * 2.0 + y
      val transformed = depthFirst(term) {
        case t if t == x => y
      }
      transformed should beStringEqual (expected)
    }
  }

  "A sum flattener" should {
    "replace nested sums with one flat sum" in {
      val x = Doubles.Var
      val term = x + x + x + x
      val expected = sum(x,x,x,x)
      val transformed = flattenSums(clean(term))
      transformed should beStringEqual (expected)
    }
  }

  "A first order sum grounder" should {
    "replace a first order sum with a propositional sum" in {
      val x = Seqs(Doubles,3).Var
      val indices = SeqConst(0,1,2)
      val term = sum(indices) {i => x(i)}
      val transformed = groundSums(term)
      val expected = varSeqSum(VarSeq(indices.length,IndexedSeq(x(indices(0)),x(indices(1)),x(indices(2)))))
      transformed should beStringEqual (expected)
      transformed.eval2(IndexedSeq(1.0,2.0,3.0)) should be (expected.eval2(IndexedSeq(1.0,2.0,3.0)))
    }
  }

  "A sample counter" should {
    "estimate the number of distinct samples a stochastic term can generate" in {
      val s1 = sampleSequential(0 until 4)
      val s2 = sampleSequential(0 until 2)
      val m1 = mem(s1)
      val m2 = mem(m1 + m1 + s2)
      val t = m2 + m2
      Traversal.guessSampleCount(t) should be (8)
    }
  }

}
