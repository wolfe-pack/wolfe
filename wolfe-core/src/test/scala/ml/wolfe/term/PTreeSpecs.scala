package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author rockt
 */
class PTreeSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  def toParse(graph: Set[(Int, Int)], slen: Int): IndexedSeq[IndexedSeq[Boolean]] = {
    for (mod <- 0 until slen) yield for (head <- 0 until slen) yield graph(head -> mod)
  }

  val trees = Seq(
    Set(0 -> 1, 1 -> 2, 2 -> 3),
    Set(0 -> 1, 1 -> 2, 1 -> 3),
    Set(0 -> 1, 1 -> 3, 3 -> 2),
    Set(0 -> 2, 2 -> 1, 2 -> 3),
    Set(0 -> 3, 3 -> 2, 2 -> 1),
    Set(0 -> 3, 3 -> 1, 1 -> 2),
    Set(0 -> 3, 3 -> 1, 3 -> 2)
  ) map (toParse(_, 4))


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
        println(tree)
        constraint.eval(parse := tree, slen := 4) should be(0.0)
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

    val length = 4
    val Parses = Seqs(Seqs(Bools, length), length)
    val slen = Ints.Var


    "evaluate marginals in a brute-force manner for a single structured parse variable" in {
      val parse = Parses.Var
      val constraint = PTree(parse, slen)
      val input = Settings(Setting.disc(length))
      val msg = Msgs(Parses.createZeroMsg())
      val marginalizer = new ExhaustiveSearchMarginalizer(constraint, Seq(parse), Seq(slen), input, msg, true)
      marginalizer.updateMessages()
      val result = Parses.toMarginals(marginalizer.outputMsgs(0))
      //the exponentiated log-marginals should correspond to counts
      //there are 7 legal parses
      println(result)

      for (m <- 0 until length; h <- 0 until length) {
        if (h == m || m == 0) {
          result(m)(h)(true) should be(Double.NegativeInfinity)
          result(m)(h)(false) should be(math.log(7) +- eps)
        }
      }
      for ((h, m) <- Seq(0 -> 1, 1 -> 2, 3 -> 2, 0 -> 3)) {
        result(m)(h)(true) should be(math.log(3) +- eps)
        result(m)(h)(false) should be(math.log(4) +- eps)
      }
      for ((h, m) <- Seq(2 -> 3, 1 -> 3, 2 -> 1, 3 -> 1)) {
        result(m)(h)(true) should be(math.log(2) +- eps)
        result(m)(h)(false) should be(math.log(5) +- eps)
      }

      for ((h, m) <- Seq(0 -> 2)) {
        result(m)(h)(true) should be(math.log(1) +- eps)
        result(m)(h)(false) should be(math.log(6) +- eps)
      }

    }

    "evaluate marginals using dynamic programming for a grounded parse variable" in {
      def nodePerEdge(h: Int, m: Int) = Bools.Variable(s"edge($h,$m)")
      val nodeMap = (for (m <- 0 until length; h <- 0 until length) yield (h, m) -> nodePerEdge(h, m)).toMap
      def nodesPerMod(m: Int) = for (h <- 0 until length) yield nodeMap(h, m)

      val nodes = for (m <- 0 until length)
        yield Parses.elementDom.Term(Parses.elementDom.lengthDom.Variable("l" + m), nodesPerMod(m))

      val parse = Parses.Term(Parses.lengthDom.Variable("length"), nodes)
      val constraint = PTree(parse, slen)

      val input = Settings(Setting.disc(length))
      val parseVars = parse.vars
      val msgs = Msgs(parseVars map (_.domain.createZeroMsg))

      val bruteForceMarginalizer = new ExhaustiveSearchMarginalizer(constraint, parseVars, Seq(slen), input, msgs, true)
      val dpMarginalizer = constraint.marginalizerImpl(parseVars, Seq(slen))(input, msgs, true) //new ExhaustiveSearchMarginalizer(constraint, parseVars, Seq(slen), input, msgs, true)

      bruteForceMarginalizer.updateMessages()(Execution(0))
      dpMarginalizer.updateMessages()(Execution(0))

      //figure out indices of nodes
      val indices = nodeMap map { case ((h, m), node) => (h, m) -> parseVars.indexOf(node) }

      val result = (for (m <- 0 until length; h <- 0 until length)
        yield (m, h) -> Bools.toMarginals(bruteForceMarginalizer.outputMsgs(indices(h, m)))).toMap

      val resultDP = (for (m <- 0 until length; h <- 0 until length)
        yield (m, h) -> Bools.toMarginals(dpMarginalizer.outputMsgs(indices(h, m)))).toMap

      for (m <- 0 until length; h <- 0 until length) {
        val margBrute = result(m, h).expNormalize
        val margDP = resultDP(m, h).expNormalize
        margDP(true) should be(margBrute(true) +- eps)
        margDP(false) should be(margBrute(false) +- eps)
      }
      val lengthBrute = Parses.lengthDom.toMarginals(bruteForceMarginalizer.outputMsgs(parseVars.indexOf(parse.length))).expNormalize
      val lengthDP = Parses.lengthDom.toMarginals(dpMarginalizer.outputMsgs(parseVars.indexOf(parse.length))).expNormalize
      lengthDP should be(lengthBrute)
    }

    "evaluate marginals within a sum product algorithm" in {
      val y = Parses.Var
      val constraint = PTree(y, slen)
      val model = constraint + I(y(1.toConst)(0.toConst)) * 1.0
      val msg = Msgs(Parses.createZeroMsg())
      val input = Settings(Setting.disc(length))
      val marginalizerBrute = new ExhaustiveSearchMarginalizer(model, Seq(y), Seq(slen), input, msg, true)

      val marginalizerBP = new SumProductBP(model, Seq(y), input, msg)(BPParameters(2, BP.Schedule.synchronized))

      //marginalizerBrute.updateMessages()(Execution(0))
      marginalizerBP.updateMessages()(Execution(0))
      marginalizerBrute.updateMessages()(Execution(0))

      val resultBP = Parses.toMarginals(marginalizerBP.outputMsgs(0))
      val resultBrute = Parses.toMarginals(marginalizerBrute.outputMsgs(0))


      for (m <- 0 until length; h <- 0 until length) {
        val margBrute = resultBrute(m)(h).expNormalize
        val margBP = resultBP(m)(h).expNormalize

        margBP(true) should be (margBrute(true) +- eps)
      }

    }

    "evaluate marginals using the marginals operator" in {

      implicit val params = BPParameters(2, BP.Schedule.synchronized)

      def model(y:Parses.Term, slen:IntTerm) = {
        I(y(1)(0)) subjectTo projectiveTree(y,slen)
      }

      val resultBP = marginals(Parses)(y => model(y,4) marginalsBy Marginalizer.sumProduct).eval()
      val resultBrute = marginals(Parses)(y => model(y,4) marginalsBy Marginalizer.bruteForce).eval()

      for (m <- 0 until length; h <- 0 until length) {
        val margBrute = resultBrute(m)(h).expNormalize
        val margBP = resultBP(m)(h).expNormalize

        margBP(true) should be (margBrute(true) +- eps)
      }





    }




  }

}
