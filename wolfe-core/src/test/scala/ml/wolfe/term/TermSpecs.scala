package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class TermSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._
  import ml.wolfe.util.Math._

  "An vector variable term" should {
    "evaluate to a vector" in {
      val x = vectors(2).Var
      val result = x.eval2(vector(1.0, 2.0))
      result should equal(vector(1.0, 2.0))
    }

    "provide its constant gradient" in {
      val x = vectors(2).Var
      val result = x.gradient2(x, vector(2.0, 1.0))
      result should equal(vector(1.0, 1.0))
    }
  }

  "A matrix variable term" should {
    "evaluate to a matrix" in {
      val X = matrices(2, 3).Var
      val tensor2 = matrix(Seq(1, 2, 3), Seq(4, 5, 6))
      val result = X.eval2(tensor2)
      result should equal(tensor2)
    }

    "provide its constant gradient" in {
      val X = matrices(2, 3).Var
      val tensor2 = matrix(Seq(1, 2, 3), Seq(4, 5, 6))
      val result = X.gradient2(X, tensor2)
      result.toArray should equal(new MatrixDom(2: Int, 3: Int).one.toArray)
    }
  }

  "A double variable term" should {
    "provide its argmax" in {
      val x = Doubles.Var
      val y = Doubles.Var
      x.argmax(x) should be(Double.PositiveInfinity)
      x.argmax(y, 2.0) should be(2.0)
    }
  }

  "A Tuple2Var term" should {
    "evaluate to a tuple2" in {
      val dom = Doubles x Doubles
      val x = dom.Var
      val result = x.eval2((1.0, 2.0))
      result should be(1.0, 2.0)
    }

    "provide its first argument" in {
      val dom = Doubles x Doubles
      val x = dom.variable("x")
      val arg1 = x._1
      arg1.eval2((2.0, 1.0)) should be(2.0)
    }
  }

  "A dot product term" should {
    "evaluate to the value of a dot product" in {
      val x = vectors(2).Var
      val dot = x dot x
      val result = dot.eval2(vector(2.0, 3.0))
      result should be(13.0)
    }

    "provide its gradient for identical variables" in {
      val x = vectors(2).Var
      val dot = x dot x
      val result = dot.gradient2(x, vector(2.0, 3.0))
      result should equal(vector(4.0, 6.0))
    }

    "provide its gradient for different variables " in {
      val x = vectors(2).Var
      val y = vectors(2).Var
      val dot = x dot y
      dot.gradient2(x, vector(2.0, 3.0), vector(1.0, 2.0)) should equal(vector(1.0, 2.0))
      dot.gradient2(y, vector(2.0, 3.0), vector(1.0, 2.0)) should equal(vector(2.0, 3.0))
    }
  }

  "A matrix-vector product term" should {
    "evaluate to the value of a matrix-vector product" in {
      val x = vectors(2).Var
      val A = matrices(3, 2).Var
      val op = A * x
      val tensor2 = matrix(Seq(1, 0), Seq(1, 1), Seq(2, 1))
      val result = op.eval2(tensor2, vector(2, 4))
      result should equal(vector(2.0, 6.0, 8.0))
    }

    "provide its gradient for different variables given an upstream error vector" ignore {
      val AVar = matrices(3, 2).Var
      val xVar = vectors(2).Var
      val term = AVar * xVar

      val A = matrix(Seq(1, 0.5), Seq(1, 1), Seq(2, 1))
      val x = vector(2, 4)
      val e = vector(1.0, 0.5, 1.5)

      val dA = term.gradient(Seq(A, x), e, Seq(AVar))(0)
      val dx = term.gradient(Seq(A, x), e, Seq(xVar))(1)

      //todo: test the gradient
      println(dx)
      println(dA)
    }
  }

  "A product" should {
    "evaluate to the product of its arguments" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val term = x * y * x * 0.5
      term.eval2(2.0, 3.0) should be(6.0)
    }
    "calculate its gradient" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val term = x * y * x * 0.5
      term.gradient2(x, 2.0, 3.0) should be(6.0)
      term.gradient2(y, 2.0, 3.0) should be(2.0)
    }
  }

  "A div" should {
    "evaluate to the division of its arguments" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val term = (x / y) * 2.0
      term.eval2(2.0, 0.5) should be(8.0)
    }
    "calculate its gradient" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val term = (x / y) * 2.0
      term.gradient2(x, 2.0, 0.5) should be(4.0)
      term.gradient2(y, 2.0, 0.5) should be(-16.0)
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
      val x = vectors(2).Var
      val y = vectors(2).Var
      val term = log(sigm(x dot y))
      term.eval2(vector(1.0, 2.0), vector(2.0, 3.0)) should equal(math.log(sigmoid(8.0)))
    }

    "provide the gradient of a logistic loss matrix factorization objective" in {
      val x = vectors(2).Var
      val y = vectors(2).Var
      val term = log(sigm(x dot y))
      val result = term.gradient2(x, vector(1.0, 2.0), vector(2.0, 3.0))
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

  "A max term" should {
    "evaluate to the maximum over its domain" in {
      val x = Bools.Var
      val term = max(Bools) { y => I(y && x)}
      term.eval(false) should be(0.0)
      term.eval(true) should be(1.0)
    }

    "provide an element of its sub-gradient" in {
      val weights = vectors(2).Var
      val term = max(Bools) { label => I(label) * (weights dot vector(1, 2))}
      term.gradient(weights, vector(1, 1)) should equal(vector(1, 2))
      term.gradient(weights, vector(1, -1)) should equal(vector(0, 0))
    }

    "maximize over a structured search space" in {
      val labels = Discretes("V", "N")
      val sequences = seqs(labels, 2)

      def model(y: sequences.DomTerm) =
        I(y(0) === labels.Const("V")) * 2.0 +
          I(y(1) === labels.Const("N")) * 1.0
      val result = max(sequences)(model)
      result.eval() should be(3.0)
    }

  }

  "A sequence term" should {
    "evaluate to a sequence" in {
      val dom = Seqs(Doubles, 3)
      val x = Doubles.Var
      val y = Doubles.Var
      val term = dom.Term(x,y,x)
      term.eval2(1.0, 2.0) should be(Seq(1.0, 2.0, 1.0))
    }

    "provide its gradient" in {
      val dom = Seqs(Doubles, 3)
      val x = Doubles.Var
      val y = Doubles.Var
      val term = dom.Term(x, y, x)
      term.gradient2(x, 10.0, 3.0) should be(2.0)
      term.gradient2(y, 10.0, 3.0) should be(1.0)
    }

  }

  "A structured loss" should {
    "evaluate to its value" in {
      implicit val Scores = seqs(Doubles, 3)
      implicit val Output = seqs(Bools, 3)

      def model(scores: Scores.Term)(y: Output.Term) =
        sum(0 until y.length) { i => I(y(i)) * scores(i)}

      def loss(gold: Output.Term)(scores: Scores.Term) =
        max(Output) {
          model(scores)
        } - model(scores)(gold)

      val term = loss(Output.Const(true, false, true))(Scores.Const(1.0, 1.0, -1.0))

      term.eval() should be(2.0)
    }

    "calculate its gradient for tied weights" in {
      implicit val Scores = seqs(Doubles, 3)
      implicit val Output = seqs(Bools, 3)

      def model(scores: Scores.Term)(y: Output.Term) =
        sum(0 until y.length) { i => I(y(i)) * scores(i)}

      def loss(gold: Output.Term)(scores: Scores.Term) =
        max(Output) {
          model(scores)
        } - model(scores)(gold)

      val weight = Doubles.variable("w")
      val weights = Scores.Term(weight, weight, weight)
      val term = loss(Output.Const(false, false, true))(weights)

      term.gradient(weight, 1.0) should be(2.0)
      term.gradient(weight, -1.0) should be(-1.0)

    }
  }

  "A quadratic objective" should {
    "provide its gradient" in {
      val x = Doubles.Var
      val obj = (x * 4.0) - (x * x)
      obj.gradient2(x, 0.0) should be(4.0)
      obj.gradient2(x, 12.0) should be(-20.0)
    }
  }

  "Adagrad" should {
    "optimize a quadratic objective" in {
      val x = Doubles.variable("x")
      def obj(x: Doubles.Term) = x * 4.0 - x * x
      val result = argmax(Doubles) { x => obj(x).argmaxBy(Argmaxer.adaGrad(100, 1))}
      result.eval() should be(2.0 +- eps)
    }
    "optimize a multivariate quadratic objective" in {
      val X = vectors(2)
      val x = X.variable("x")
      def obj(x: X.Term) = (x dot vector(4.0, 4.0)) - (x dot x)
      val result = argmax(X) { x => obj(x).argmaxBy(Argmaxer.adaGrad(100, 1))}
      result.eval() should equal(vector(2.0, 2.0))
    }
  }

  "Exhaustive max-marginalizing" should {
    "provide the exact max marginals" in {
      val x = Bools.variable("x")
      val y = Bools.variable("y")
      val term = I(x === y)
      val result = term.maxMarginals(x, y)(IndexedSeq(1.0, 2.0))()
      result should be(IndexedSeq(2.0, 3.0))
    }
  }

  "A typed term" should {
    "have a typed apply method" in {
      val x = Doubles.variable("x")
      val term: Double => Double = (x * x) typed Doubles
      term(2.0) should be(4.0)
    }
  }


}
