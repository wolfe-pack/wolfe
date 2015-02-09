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
      val x = vectors(2).variable("x")
      val result = x.eval(vector(1.0, 2.0))
      result should equal(vector(1.0, 2.0))
    }

    "provide its constant gradient" in {
      val x = vectors(2).variable("x")
      val result = x.gradient(x, vector(2.0, 1.0))
      result should equal(vector(1.0, 1.0))
    }
  }

  "A matrix variable term" should {
    "evaluate to a matrix" in {
      val X = matrices(2, 3).variable("X")
      val tensor2 = matrix(Seq(1, 2, 3), Seq(4, 5, 6))
      val result = X.eval(tensor2)
      result should equal(tensor2)
    }

    "provide its constant gradient" in {
      val X = matrices(2, 3).variable("X")
      val tensor2 = matrix(Seq(1, 2, 3), Seq(4, 5, 6))
      val result = X.gradient(X, tensor2)
      result.toArray should equal(new MatrixDom(2: Int, 3: Int).one.toArray)
    }
  }

  "A double variable term" should {
    "provide its argmax" in {
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      x.argmax(x) should be(Double.PositiveInfinity)
      x.argmax(y, 2.0) should be(2.0)
    }
  }

  "A Tuple2Var term" should {
    "evaluate to a tuple2" in {
      val dom = doubles x doubles
      val x = dom.variable("x")
      val result = x.eval((1.0, 2.0))
      result should be(1.0, 2.0)
    }

    "provide its first argument" in {
      val dom = doubles x doubles
      val x = dom.variable("x")
      val arg1 = x._1
      arg1.eval((2.0, 1.0)) should be(2.0)
    }
  }

  "A dot product term" should {
    "evaluate to the value of a dot product" in {
      val x = vectors(2).variable("x")
      val dot = x dot x
      val result = dot.eval(vector(2.0, 3.0))
      result should be(13.0)
    }

    "provide its gradient for identical variables" in {
      val x = vectors(2).variable("x")
      val dot = x dot x
      val result = dot.gradient(x, vector(2.0, 3.0))
      result should equal(vector(4.0, 6.0))
    }

    "provide its gradient for different variables " in {
      val x = vectors(2).variable("x")
      val y = vectors(2).variable("y")
      val dot = x dot y
      dot.gradient(x, vector(2.0, 3.0), vector(1.0, 2.0)) should equal(vector(1.0, 2.0))
      dot.gradient(y, vector(2.0, 3.0), vector(1.0, 2.0)) should equal(vector(2.0, 3.0))
    }
  }

  "A matrix-vector product term" should {
    "evaluate to the value of a matrix-vector product" in {
      val x = vectors(2).variable("x")
      val A = matrices(3, 2).variable("A")
      val op = A * x
      val tensor2 = matrix(Seq(1, 0), Seq(1, 1), Seq(2, 1))
      val result = op.eval(tensor2, vector(2, 4))
      result should equal(vector(2.0, 6.0, 8.0))
    }

    "provide its gradient for different variables given an upstream error vector" in {
      val AVar = matrices(3,2).variable("A")
      val xVar = vectors(2).variable("x")
      val term = AVar * xVar

      val A = matrix(Seq(1, 0.5), Seq(1, 1), Seq(2, 1))
      val x = vector(2, 4)
      val e = vector(1.0, 0.5, 1.5)

      val dA = term.gradient(Seq(A, x), e, Seq(AVar))(0)
      val dx = term.gradient(Seq(A, x), e, Seq(xVar))(1)

      println(dx)
      println(dA)
    }
  }

  "A sum" should {
    "evaluate to the sum of its arguments" in {
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      val term = x + y + x
      term.eval(1.0, 2.0) should be(4.0)
    }

    "calculate its gradient" in {
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      val term = x + y + x
      term.gradient(y, 10.0, 5.0) should be(1.0)
      term.gradient(x, 10.0, 5.0) should be(2.0)
    }

    "calculate sequential gradients" in {
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      val term = (x * 2.0 + y * 3.0).sequentialGradient()
      val diff = term.differentiator(Seq(x))
      diff.gradient(x, 1.0, 1.0) should be(2.0)
      diff.gradient(x, 1.0, 1.0) should be(0.0)
      diff.gradient(x, 1.0, 1.0) should be(2.0)

    }

    "should be expressable through the sum operator" in {
      val dom = seqs(bools, 3)
      def model(y: dom.DomTerm) = sum(0 until dom.length) { i => I(y(i))}
      val y = dom.variable("y")
      model(y).eval(IndexedSeq(false, true, true)) should be(2.0)
    }


  }

  "A product" should {
    "evaluate to the product of its arguments" in {
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      val term = x * y * x * 0.5
      term.eval(2.0, 3.0) should be(6.0)
    }
    "calculate its gradient" in {
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      val term = x * y * x * 0.5
      term.gradient(x, 2.0, 3.0) should be(6.0)
      term.gradient(y, 2.0, 3.0) should be(2.0)
    }
  }

  "An iverson bracket" should {
    "evaluate to 0 if a predicate is false, and 1 otherwise" in {
      val x = bools.variable("x")
      val term = I(x)
      term.eval(false) should be(0.0)
      term.eval(true) should be(1.0)
    }
  }

  "Composing log, sigmoid and dot products" should {
    "provide a logistic loss matrix factorization objective" in {
      val x = vectors(2).variable("x")
      val y = vectors(2).variable("y")
      val term = log(sigm(x dot y))
      term.eval(vector(1.0, 2.0), vector(2.0, 3.0)) should equal(math.log(sigmoid(8.0)))
    }

    "provide the gradient of a logistic loss matrix factorization objective" in {
      val x = vectors(2).variable("x")
      val y = vectors(2).variable("y")
      val term = log(sigm(x dot y))
      val result = term.gradient(x, vector(1.0, 2.0), vector(2.0, 3.0))
      val prob = sigmoid(8.0)
      result should equal(vector(2.0, 3.0) * (1.0 - prob))
    }

    "provide a full matrix factorization objective" in {
      val k = 2
      val m = 3
      val n = 3
      val Params = seqs(vectors(k), m) x seqs(vectors(k), n)

      def cell(scale: Double)(a: VectorTerm, v: VectorTerm) =
        log(sigm((a dot v) * scale)) + (a dot v) * 0.1

      def loss(positive: Seq[(Int, Int)], negative: Seq[(Int, Int)])(e: Params.Term) = {

        sum(positive) { case (i, j) => cell(1.0)(e._1(i), e._2(j))} +
        sum(negative) { case (i, j) => cell(-1.0)(e._1(i), e._2(j))}

      }


//      def implicitFeedbackLoss(positive: Seq[(Int, Int)])(e: Params.Variable) =
//        sum(positive) { case (i, j) =>
//          cell(1.0)(e._1(i), e._2(j)) +
//          sum(sequential(0 until m)) { k => cell(-1.0)(e._1(i), e._2(k))}
//        }

    }

  }

  "A term with discrete variables" should {
    "provide its argmax" in {
      val dom = bools x bools
      val x = dom.variable("x")
      val result = argmax(dom) { x => I(x._1 && x._2)}
      result.eval() should be(true, true)
    }
    "provide a partial argmax" in {
      val y = bools.variable("y")
      val term = argmax(bools)(x => I(x === y))
      term.eval(true) should be(true)
      term.eval(false) should be(false)
    }
  }

  "A max term" should {
    "evaluate to the maximum over its domain" in {
      val x = bools.variable("x")
      val term = max(bools) { y => I(y && x)}
      term.eval(false) should be(0.0)
      term.eval(true) should be(1.0)
    }

    "provide an element of its sub-gradient" in {
      val weights = vectors(2).variable("w")
      val term = max(bools) { label => I(label) * (weights dot vector(1, 2))}
      term.gradient(weights, vector(1, 1)) should equal(vector(1, 2))
      term.gradient(weights, vector(1, -1)) should equal(vector(0, 0))
    }

    "maximize over a structured search space" in {
      val labels = discrete("V", "N")
      val sequences = seqs(labels, 2)

      def model(y: sequences.DomTerm) =
        I(y(0) === labels.const("V")) * 2.0 +
        I(y(1) === labels.const("N")) * 1.0
      val result = max(sequences) {model}
      result.eval() should be(3.0)
    }

  }

  "A sequence term" should {
    "evaluate to a sequence" in {
      val dom = seqs(doubles, 3)
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      val term = seq(dom)(x, y, x)
      term.eval(1.0, 2.0) should be(Seq(1.0, 2.0, 1.0))
    }

    "provide its gradient" in {
      val dom = seqs(doubles, 3)
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      val term = seq(dom)(x, y, x)
      term.gradient(x, 10.0, 3.0) should be(2.0)
      term.gradient(y, 10.0, 3.0) should be(1.0)
    }

  }

  "A structured loss" should {
    "evaluate to its value" in {
      implicit val Scores = seqs(doubles, 3)
      implicit val Output = seqs(bools, 3)

      def model(scores: Scores.Term)(y: Output.Term) =
        sum(0 until y.length) { i => I(y(i)) * scores(i)}

      def loss(gold: Output.Term)(scores: Scores.Term) =
        max(Output) {model(scores)} - model(scores)(gold)

      val term = loss(Output.Const(true, false, true))(Scores.Const(1.0, 1.0, -1.0))

      term.eval() should be(2.0)
    }

    "calculate its gradient for tied weights" in {
      implicit val Scores = seqs(doubles, 3)
      implicit val Output = seqs(bools, 3)

      def model(scores: Scores.Term)(y: Output.Term) =
        sum(0 until y.length) { i => I(y(i)) * scores(i)}

      def loss(gold: Output.Term)(scores: Scores.Term) =
        max(Output) {model(scores)} - model(scores)(gold)

      val weight = doubles.variable("w")
      val weights = Scores.Term(weight, weight, weight)
      val term = loss(Output.Const(false, false, true))(weights)

      term.gradient(weight, 1.0) should be(2.0)
      term.gradient(weight, -1.0) should be(-1.0)

    }
  }

  "A quadratic objective" should {
    "provide its gradient" in {
      val x = doubles.variable("x")
      val obj = (x * 4.0) - (x * x)
      obj.gradient(x, 0.0) should be(4.0)
      obj.gradient(x, 12.0) should be(-20.0)

    }
    "support custom argmaxing" in {
      val x = doubles.variable("x")
      def obj(x: doubles.Term) = x * 4.0 - x * x
      val result = argmax(doubles) { x => obj(x).argmaxBy(Argmaxer.ascent(100, 0.1))}
      result.eval() should be(2.0 +- eps)
    }
  }

  "Adagrad" should {
    "optimize a quadratic objective" in {
      val x = doubles.variable("x")
      def obj(x: doubles.Term) = x * 4.0 - x * x
      val result = argmax(doubles) { x => obj(x).argmaxBy(Argmaxer.adaGrad(100, 1))}
      result.eval() should be(2.0 +- eps)
    }
    "optimize a multivariate quadratic objective" in {
      val X = vectors(2)
      val x = X.variable("x")
      def obj(x: X.Term) = (x dot vector(4.0,4.0)) - (x dot x)
      val result = argmax(X) { x => obj(x).argmaxBy(Argmaxer.adaGrad(100, 1))}
      result.eval() should equal(vector(2.0,2.0))
    }
  }

  "A lazy sum" should {
    "return a different value every time when evaluated" in {
      val n = 3
      val Y = seqs(doubles, n)
      val y = Y.variable("y")
      val term = sum(sequential(0 until n)) { i => y(i)}
      for (_ <- 0 until 2; i <- 0 until n) term.eval(IndexedSeq(1.0, 2.0, 3.0)) should be(i + 1.0)
    }
    "return a different gradient every time when evaluated" in {
      val n = 3
      val Y = seqs(doubles, n)
      val y = Y.variable("y")
      val term = sum(sequential(0 until n)) { i => y(i) * y(i)}
      for (_ <- 0 until 2; i <- 0 until n) {
        val gradient = term.gradient(y, IndexedSeq(0.0, 1.0, 2.0))
        for (j <- 0 until n) gradient(j) should be(if (j == i) 2.0 * i else 0.0)
      }
    }

  }

  "Exhaustive max-marginalizing" should {
    "provide the exact max marginals" in {
      val x = bools.variable("x")
      val y = bools.variable("y")
      val term = I(x === y)
      val result = term.maxMarginals(x,y)(IndexedSeq(1.0,2.0))()
      result should be (IndexedSeq(2.0,3.0))
    }
  }

  "A typed term" should {
    "have a typed apply method" in {
      val x = doubles.variable("x")
      val term: Double => Double = (x * x) typed doubles
      term(2.0) should be(4.0)
    }
  }

}
