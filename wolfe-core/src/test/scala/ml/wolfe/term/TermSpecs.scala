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
      val result = x(vector(1.0, 2.0))
      result should equal(vector(1.0, 2.0))
    }

    "provide its constant gradient" in {
      val x = vectors(2).variable("x")
      val result = x.gradient(x, vector(2.0, 1.0))
      result should equal(vector(1.0, 1.0))
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
      val result = x((1.0, 2.0))
      result should be(1.0, 2.0)
    }

    "provide its first argument" in {
      val dom = doubles x doubles
      val x = dom.variable("x")
      val arg1 = x._1
      arg1((2.0, 1.0)) should be(2.0)
    }
  }

  "A dot product term" should {
    "evaluate to the value of a dot product" in {
      val x = vectors(2).variable("x")
      val dot = x dot x
      val result = dot(vector(2.0, 3.0))
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

  "A sum" should {
    "evaluate to the sum of its arguments" in {
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      val term = x + y + x
      term(1.0, 2.0) should be(4.0)
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
      diff.gradient(x, 1.0, 1.0) should be (2.0)
      diff.gradient(x, 1.0, 1.0) should be (0.0)
      diff.gradient(x, 1.0, 1.0) should be (2.0)

    }

    "should be expressable through the sum operator" in {
      val dom = seqs(bools, 3)
      def model(y: dom.Term) = sum(0 until dom.length) { i => I(y(i))}
      val y = dom.variable("y")
      model(y)(IndexedSeq(false, true, true)) should be(2.0)
    }


  }

  "A product" should {
    "evaluate to the product of its arguments" in {
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      val term = x * y * x * 0.5
      term(2.0, 3.0) should be(6.0)
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
      term(false) should be(0.0)
      term(true) should be(1.0)
    }
  }

  "Composing log, sigmoid and dot prodcuts" should {
    "provide a logistic loss matrix factorization objective" in {
      val x = vectors(2).variable("x")
      val y = vectors(2).variable("y")
      val term = log(sigm(x dot y))
      term(vector(1.0, 2.0), vector(2.0, 3.0)) should equal(math.log(sigmoid(8.0)))
    }

    "provide the gradient of a logistic loss matrix factorization objective" in {
      val x = vectors(2).variable("x")
      val y = vectors(2).variable("y")
      val term = log(sigm(x dot y))
      val result = term.gradient(x, vector(1.0, 2.0), vector(2.0, 3.0))
      val prob = sigmoid(8.0)
      result should equal(vector(2.0, 3.0) * (1.0 - prob))
    }
  }

  "A term with discrete variables" should {
    "provide its argmax" in {
      val result = argmax(bools x bools) { x => I(x._1 && x._2)}
      result() should be(true, true)
    }
    "provide a partial argmax" in {
      val y = bools.variable("y")
      val term = argmax(bools)(x => I(x === y))
      term(true) should be(true)
      term(false) should be(false)
    }
  }

  "A max term" should {
    "evaluate to the maximum over its domain" in {
      val x = bools.variable("x")
      val term = max(bools) { y => I(y && x)}
      term(false) should be(0.0)
      term(true) should be(1.0)
    }

    "provide an element of its sub-gradient" in {
      val weights = vectors(2).variable("w")
      val term = max(bools) { label => I(label) * (weights dot vector(1, 2))}
      term.gradient(weights, vector(1, 1)) should equal(vector(1, 2))
      term.gradient(weights, vector(1, -1)) should equal(vector(0, 0))
    }

    "maximize over a structured search space" in {
      implicit val labels = discrete("V", "N")
      val sequences = seqs(labels, 2)
      def model(y: sequences.Term) =
        I(y(0) === "V") * 2.0 +
        I(y(1) === "N") * 1.0
      val result = max(sequences) {model}
      result() should be(3.0)
    }

  }

  "A sequence term" should {
    "evaluate to a sequence" in {
      val dom = seqs(doubles, 3)
      val x = doubles.variable("x")
      val y = doubles.variable("y")
      val term = seq(dom)(x, y, x)
      term(1.0, 2.0) should be(Seq(1.0, 2.0, 1.0))
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

      def loss(gold:Output.Term)(scores:Scores.Term) =
        max(Output) { model(scores)} - model(scores)(gold)

      val term = loss(IndexedSeq(true,false,true))(IndexedSeq(1.0,1.0,-1.0))

      term() should be (2.0)
    }

    "calculate its gradient for tied weights" in {
      implicit val Scores = seqs(doubles, 3)
      implicit val Output = seqs(bools, 3)

      def model(scores: Scores.Term)(y: Output.Term) =
        sum(0 until y.length) { i => I(y(i)) * scores(i)}

      def loss(gold:Output.Term)(scores:Scores.Term) =
        max(Output) { model(scores)} - model(scores)(gold)

      val weight = doubles.variable("w")
      val weights = Scores(weight,weight,weight)
      val term = loss(IndexedSeq(false,false,true))(weights)

      term.gradient(weight, 1.0) should be (2.0)
      term.gradient(weight, -1.0) should be (-1.0)

    }
  }

  "A quadratic objective" should {
    "provide its gradient" in {
      val x = doubles.variable("x")
      val obj = (x * 4.0) - (x * x)
      obj.gradient(x,0.0) should be (4.0)
      obj.gradient(x,12.0) should be (-20.0)

    }
    "support custom argmaxing" in {
      val x = doubles.variable("x")
      def obj(x:doubles.Term) = x * 4.0 - x * x
      val result = argmax(doubles) { x => obj(x).argmaxBy(Argmaxer.ascent(100,0.1))}
      result() should be (2.0 +- eps)
    }

  }



}
