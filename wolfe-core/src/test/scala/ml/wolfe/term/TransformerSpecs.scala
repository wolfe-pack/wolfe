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
      transformed should beStringEqual(expected)
    }
  }

  "A reusing depth first transformer" should {
    "return identical transformed terms for identical input terms" in {
      val x = Doubles.Var
      val t = x * x + x
      val transformed = depthFirstAndReuse(t) {
        case `x` => 2.0
      }
      val expected = 2.0.toConst * 2.0.toConst + 2.0.toConst
      transformed._1 should beStringEqual(expected)
      val Sum(Vector(Product(Vector(c1, c2)), c3)) = transformed._1
      (c1 eq c2) should be(true)
      (c1 eq c3) should be(true)
    }
  }

  "A reusing depth last transformer" should {
    "return identical transformed terms for identical input terms" in {
      val x = Doubles.Var
      val t = x * x + x
      val transformed = depthLastAndReuse(t) {
        case `x` => 2.0
      }
      val expected = 2.0.toConst * 2.0.toConst + 2.0.toConst
      transformed._1 should beStringEqual(expected)
      val Sum(Vector(Product(Vector(c1, c2)), c3)) = transformed._1
      (c1 eq c2) should be(true)
      (c1 eq c3) should be(true)
    }
  }


  "A sum flattener" should {
    "replace nested sums with one flat sum" in {
      val x = Doubles.Var
      val term = x + x + x + x
      val expected = sum(x, x, x, x)
      val transformed = flattenSums(clean(term))
      transformed should beStringEqual(expected)
    }
  }

  "A first order sum grounder" should {
    "replace a first order sum with a propositional sum" in {
      val x = Seqs(Doubles, 3).Var
      val indices = SeqConst(0, 1, 2)
      val term = sum(indices) { i => x(i) }
      val transformed = groundSums(term)
      val i = Ints.variable("_i")
      val expected = sum(indices.length)(x(0), x(1), x(2))
      transformed should beStringEqual(expected)
      transformed.eval(x := IndexedSeq(1.0, 2.0, 3.0)) should be(expected.eval(x := IndexedSeq(1.0, 2.0, 3.0)))
    }
  }

  "A sample counter" should {
    "estimate the number of distinct samples a stochastic term can generate" in {
      val s1 = sampleSequential(0 until 4)
      val s2 = sampleSequential(0 until 2)
      val m1 = mem(s1)
      val m2 = mem(m1 + m1 + s2)
      val t = m2 + m2
      Traversal.distinctSampleCount(t) should be(8)
    }
  }


}
