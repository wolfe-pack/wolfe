package ml.wolfe.term

import ml.wolfe.{FactorieVector, WolfeSpec}

/**
 * @author riedel
 */
class TermSpecs extends WolfeSpec {
  import ml.wolfe.term.TermImplicits._
  import ml.wolfe.util.Math._

  "An vector variable term" should {
    "evaluate to a vector" in {
      val x = Vectors(2).Var
      val result = x.eval(vector(1.0, 2.0))
      result should equal(vector(1.0, 2.0))
    }

    "provide its constant gradient" in {
      val x = Vectors(2).Var
      val result = x.gradient(x, vector(2.0, 1.0))
      result should equal(vector(1.0, 1.0))
    }
  }

  "A matrix variable term" should {
    "evaluate to a matrix" in {
      val X = Matrices(2, 3).Var
      val tensor2 = matrix(Seq(1, 2, 3), Seq(4, 5, 6))
      val result = X.eval(tensor2)
      result should equal(tensor2)
    }

    "provide its constant gradient" in {
      val X = Matrices(2, 3).Var
      val tensor2 = matrix(Seq(1, 2, 3), Seq(4, 5, 6))
      val result = X.gradient(X, tensor2)
      result.toArray should equal(new MatrixDom(2: Int, 3: Int).one.toArray)
    }
  }


  "A Tuple2Var term" should {
    "evaluate to a tuple2" in {
      val dom = Doubles x Doubles
      val x = dom.Var
      val result = x.eval((1.0, 2.0))
      result should be(1.0, 2.0)
    }

    "provide its first argument" ignore {
      val dom = Doubles x Doubles
      val x = dom.Var
      val arg1 = x._1
      arg1.eval((2.0, 1.0)) should be(2.0)
    }
  }

  "A dot product term" should {
    "evaluate to the value of a dot product" in {
      val x = Vectors(2).Var
      val dot = x dot x
      val result = dot.eval(vector(2.0, 3.0))
      result should be(13.0)
    }

    "provide its gradient for identical variables" in {
      val x = Vectors(2).Var
      val dot = x dot x
      val result = dot.gradient(x, vector(2.0, 3.0))
      result should equal(vector(4.0, 6.0))
    }

    "provide its gradient for different variables " in {
      val x = Vectors(2).Var
      val y = Vectors(2).Var
      val dot = x dot y
      dot.gradient(x, vector(2.0, 3.0), vector(1.0, 2.0)) should equal(vector(1.0, 2.0))
      dot.gradient(y, vector(2.0, 3.0), vector(1.0, 2.0)) should equal(vector(2.0, 3.0))
    }
  }

  "A matrix-vector product term" should {
    "evaluate to the value of a matrix-vector product" in {
      val x = Vectors(2).Var
      val A = Matrices(3, 2).Var
      val op = A * x
      val tensor2 = matrix(Seq(1, 0), Seq(1, 1), Seq(2, 1))
      val result = op.eval(tensor2, vector(2, 4))
      result should equal(vector(2.0, 6.0, 8.0))
    }

    "provide its gradient for different variables given an upstream error vector" ignore {
      val AVar = Matrices(3, 2).Var
      val xVar = Vectors(2).Var
      val term = AVar * xVar

      val A = matrix(Seq(1, 0.5), Seq(1, 1), Seq(2, 1))
      val x = vector(2, 4)
      val e = vector(1.0, 0.5, 1.5)

//      val dA = term.gradient(Seq(A, x), e, Seq(AVar))(0)
//      val dx = term.gradient(Seq(A, x), e, Seq(xVar))(1)
//
//      //todo: test the gradient
//      println(dx)
//      println(dA)
    }
  }

  "A product" should {
    "evaluate to the product of its arguments" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val term = x * y * x * 0.5
      term.eval(2.0, 3.0) should be(6.0)
    }
    "calculate its gradient" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val term = x * y * x * 0.5
      term.gradient(x, 2.0, 3.0) should be(6.0)
      term.gradient(y, 2.0, 3.0) should be(2.0)
    }
  }

  "A div" should {
    "evaluate to the division of its arguments" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val term = (x / y) * 2.0
      term.eval(2.0, 0.5) should be(8.0)
    }
    "calculate its gradient" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val term = (x / y) * 2.0
      term.gradient(x, 2.0, 0.5) should be(4.0)
      term.gradient(y, 2.0, 0.5) should be(-16.0)
    }
  }

  "An iverson bracket" should {
    "evaluate to 0 if a predicate is false, and 1 otherwise" in {
      val x = Bools.Var
      val term = I(x)
      term.eval(false) should be(0.0)
      term.eval(true) should be(1.0)
    }
  }

  "Composing log, sigmoid and dot products" should {
    "provide a logistic loss matrix factorization objective" in {
      val x = Vectors(2).Var
      val y = Vectors(2).Var
      val term = log(sigm(x dot y))
      term.eval(vector(1.0, 2.0), vector(2.0, 3.0)) should equal(math.log(sigmoid(8.0)))
    }

    "provide the gradient of a logistic loss matrix factorization objective" in {
      val x = Vectors(2).Var
      val y = Vectors(2).Var
      val term = log(sigm(x dot y))
      val result = term.gradient(x, vector(1.0, 2.0), vector(2.0, 3.0))
      val prob = sigmoid(8.0)
      result should equal(vector(2.0, 3.0) * (1.0 - prob))
    }

  }

  "A term with discrete variables" should {
    "provide its argmax" in {
      val dom = Bools x Bools
      val x = dom.Var
      val result = argmax(dom) { x => I(x._1 && x._2)}
      result.eval() should be(true, true)
    }
    "provide a partial argmax" in {
      val y = Bools.Var
      val term = argmax(Bools)(x => I(x === y))
      term.eval(true) should be(true)
      term.eval(false) should be(false)
    }
  }


  "A sequence term" should {
    "evaluate to a sequence" in {
      val dom = Seqs(Doubles, 3)
      val x = Doubles.Var
      val y = Doubles.Var
      val term = dom.Term(x, y, x)
      term.eval(1.0, 2.0) should be(Seq(1.0, 2.0, 1.0))
    }

    "provide its gradient" in {
      val dom = Seqs(Doubles, 3)
      val x = Doubles.Var
      val y = Doubles.Var
      val term = dom.Term(x, y, x)
      term.gradient(x, 10.0, 3.0) should be(2.0)
      term.gradient(y, 10.0, 3.0) should be(1.0)
    }

  }

  "A structured loss" should {
    "evaluate to its value" ignore {
      implicit val Scores = Seqs(Doubles, 3)
      implicit val Output = Seqs(Bools, 3)

      def model(scores: Scores.Term)(y: Output.Term) =
        sum(0 until y.length) { i => I(y(i)) * scores(i)}

      def loss(gold: Output.Term)(scores: Scores.Term) =
        max(Output) {
          model(scores)
        } - model(scores)(gold)

      val term = loss(Output.Const(IndexedSeq(true, false, true)))(Scores.Const(IndexedSeq(1.0, 1.0, -1.0)))

      term.eval() should be(2.0)
    }

    "calculate its gradient for tied weights" ignore {
      implicit val Scores = Seqs(Doubles, 3)
      implicit val Output = Seqs(Bools, 3)

      def model(scores: Scores.Term)(y: Output.Term) =
        sum(0 until y.length) { i => I(y(i)) * scores(i)}

      def loss(gold: Output.Term)(scores: Scores.Term) =
        max(Output) {
          model(scores)
        } - model(scores)(gold)

      val weight = Doubles.Var
      val weights = Scores.Term(weight, weight, weight)
      val term = loss(Output.Const(IndexedSeq(false, false, true)))(weights)

      term.gradient(weight, 1.0) should be(2.0)
      term.gradient(weight, -1.0) should be(-1.0)

    }
  }

  "A quadratic objective" should {
    "provide its gradient" in {
      val x = Doubles.Var
      val obj = (x * 4.0) - (x * x)
      obj.gradient(x, 0.0) should be(4.0)
      obj.gradient(x, 12.0) should be(-20.0)
    }
  }


  "Exhaustive max-marginalizing" should {
    "provide the exact max marginals" in {
      val x = Bools.Var
      val y = Bools.Var
      val term = I(x === y)
      val result = term.maxMarginals(x, y)(Map(false -> 1.0,  true -> 2.0))()
      result should be(Map(false -> 2.0, true -> 3.0))
    }
  }

  "A binding" should {
    "bind variables to the value of a different term" in {
      val x = Doubles.Var
      val t = (x + 1.0)| x << 2.0
      t.eval() should be(3.0)
    }
    "bind variables to the value of a different term in a nested way" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val t = x | x << (y + 1.0) | y << 2.0
      t.eval() should be(3.0)
    }

  }

  "A choice term" should {
    "evaluate to the right branch depending on the condition" in {
      val x = Ints.Var
      val t = choice(x)(1.0.toConst, 2.0.toConst)
      t.eval(0) should be(1.0)
      t.eval(1) should be(2.0)
    }
  }

  "An ifthenelse term" should {
    "evaluate to the right branch depending on the condition" in {
      val x = Bools.Var
      val t = ifThenElse(x)(1.0.toConst)(2.0)
      t.eval(true) should be(1.0)
      t.eval(false) should be(2.0)
    }
  }


}
