package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class TransformerSpecs extends WolfeSpec {

  import TermImplicits._

  "A depth first transformer" should {
    "transform a term tree in a depth first fashion" in {
      val x = doubles.Var
      val y = doubles.Var
      val term = x * 2.0 + x
      val expected = y * 2.0 + y
      val transformed = Transformer.depthFirst(term) {
        case t if t == x => y
      }
      transformed should beStringEqual (expected)
    }
  }

  "A sum flattener" should {
    "replace nested sums with one flat sum" in {
      val x = doubles.Var
      val term = x + x + x + x
      val expected = sum(x,x,x,x)
      val transformed = Transformer.flattenSums(term)
      transformed should beStringEqual (expected)
    }
  }

  "A first order sum grounder" should {
    "replace a first order sum with a propositional sum" in {
      val x = fixedLengthSeqs(doubles,3).Var
      val indices = IndexedSeqConst(0,1,2)
      val term = sum(indices) {i => x(i)}
      val transformed = Transformer.groundSums(term)
      val expected = varSeqSum(VarSeq(indices.length,IndexedSeq(x(indices(0)),x(indices(1)),x(indices(2)))))
      transformed should beStringEqual (expected)
      //transformed.eval2(IndexedSeq(1.0,2.0,3.0)) should be (expected.eval2(IndexedSeq(1.0,2.0,3.0)))
    }
  }

}
