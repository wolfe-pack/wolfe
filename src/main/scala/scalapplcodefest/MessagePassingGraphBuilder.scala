package scalapplcodefest

import cc.factorie.maths.ArrayOps

/**
 * Takes a term and builds a message passing graph for it.
 * @author Sebastian Riedel
 */
object MessagePassingGraphBuilder {

  import MessagePassingFactorGraph._

  class TermAlignedFG(val term: Term[Double], val weights: Variable[Vector]) {
    val vars = term.variables.toSeq.filter(_ != weights).sorted(VariableOrdering)
    val graph = new MessagePassingFactorGraph
    def createVariableMapping(variable: Variable[Any]) = {
      val domain = variable.domain.eval().right.get.toSeq
      val indexOfValue = domain.zipWithIndex.toMap
      val node = graph.addNode(domain.size)
      VariableMapping(variable, node, domain, indexOfValue)
    }
    val variableMappings = vars.map(createVariableMapping)
    val variable2Mapping = variableMappings.map(m => m.variable -> m).toMap
    val dims = variableMappings.map(_.dom.size)
    val entryCount = dims.product
    lazy val node2variable = variableMappings.map(m => m.node -> m.variable).toMap

    def beliefToState() = {
      val map = for (v <- variableMappings) yield {
        val winner = ArrayOps.maxIndex(v.node.b)
        val value = v.dom(winner)
        v.variable -> value
      }
      State(map.toMap)
    }

    class FGPrinter(index:Index) extends MessagePassingFactorGraph.FGPrinter {
      def node2String(node: Node) = node2variable(node).toString
      def factor2String(factor: Factor) = ""
      def vector2String(vector: scalapplcodefest.Vector) = index.vectorToString(vector, " ")
    }
  }

  case class VariableMapping(variable: Variable[Any], node: Node, dom: Seq[Any], indexOfValue: Map[Any, Int])


  case class BuiltFactor(factor: Factor, vars: Seq[VariableMapping])

  def buildTableFactor(aligned: TermAlignedFG, term: Term[Double]) = {
    val vars = term.variables.toSeq.filter(_ != aligned.weights).sorted(VariableOrdering)
    val mappings = vars.map(aligned.variable2Mapping)
    val dims = mappings.view.map(_.dom.size).toArray
    val entryCount = dims.view.product

    //create a factor using term and all variables
    val settings = Array.ofDim[Array[Int]](entryCount)
    val scores = Array.ofDim[Double](entryCount)

    //iterate over possible states, get the state index
    for (state <- State.allStates(vars.toList)) {
      val setting = Array.ofDim[Int](vars.size)
      val score = term.eval(state).right.get
      var stateIndex = 0
      for ((v, i) <- mappings.zipWithIndex) {
        val valueIndex = v.indexOfValue(state(v.variable))
        stateIndex = valueIndex + v.dom.size * stateIndex
        setting(i) = valueIndex
      }
      settings(stateIndex) = setting
      scores(stateIndex) = score
    }
    val f = aligned.graph.addFactor(scores, settings, dims)
    BuiltFactor(f, mappings)
  }

  def buildLinearFactor(aligned: TermAlignedFG, term: Term[Double], feats: Term[Vector], condition: State = State.empty) = {
    val vars = term.variables.toSeq.filter(_ != aligned.weights).sorted(VariableOrdering)
    val mappings = vars.map(aligned.variable2Mapping)
    val dims = mappings.view.map(_.dom.size).toArray
    val entryCount = dims.view.product

    //create a factor using term and all variables
    val settings = Array.ofDim[Array[Int]](entryCount)
    val stats = Array.ofDim[Vector](entryCount)

    //iterate over possible states, get the state index
    for (state <- State.allStates(vars.toList)) {
      val setting = Array.ofDim[Int](vars.size)
      val feat = feats.eval(state + condition).right.get
      var stateIndex = 0
      for ((v, i) <- mappings.zipWithIndex) {
        val valueIndex = v.indexOfValue(state(v.variable))
        stateIndex = valueIndex + v.dom.size * stateIndex
        setting(i) = valueIndex
      }
      settings(stateIndex) = setting
      stats(stateIndex) = feat
    }
    val f = aligned.graph.addLinearFactor(stats, settings, dims)
    BuiltFactor(f, mappings)
  }


  def buildFactor(aligned: TermAlignedFG, term: Term[Double], weights: Variable[Vector]): BuiltFactor = {
    term match {
      case l@LinearModel(feats, w, base) if w == weights => buildLinearFactor(aligned, l, feats) //todo: do something with base
      case c@Conditioned(Math.Dot.Applied2(feats, w), cond) if w == weights => buildLinearFactor(aligned, c, feats, cond)
      case t => buildTableFactor(aligned, t)
    }
  }

  def build(term: Term[Double], weights: Variable[Vector] = null): TermAlignedFG = {
    val aligned = new TermAlignedFG(term, weights)

    term match {
      case Reduce(ConstantFun(Math.DoubleAdd), SeqTerm(args)) =>
        for (arg <- args) {
          val f = buildFactor(aligned, arg, weights)
          for (mapping <- f.vars) {
            aligned.graph.addEdge(f.factor, mapping.node)
          }
        }
      case _ =>
        val f = buildFactor(aligned, term, weights)
        for (mapping <- f.vars) {
          aligned.graph.addEdge(f.factor, mapping.node)
        }
    }

    aligned.graph.build()
    aligned
  }

  def main(args: Array[String]) {
    import TermImplicits._
    val r = 'r of (0 ~~ 2 |-> Bools)
    val s = 's of (0 ~~ 2 |-> Bools)

    val f = dsum(for (i <- 0 ~~ 1) yield I(!(r(i) || s(i))))

    println(f.eval(r.atom(0) -> true, s.atom(0) -> false))

    val fg = build(f)

    MaxProduct.run(fg.graph, 1)
    println(fg.beliefToState().toPrettyString)

    val key = new Index
    val feat = vsum(for (i <- 0 ~~ 2) yield e_(key(r(i), s(i))))

    val vec = feat.eval(r.atom(0) -> true, s.atom(0) -> false, r.atom(1) -> false, s.atom(1) -> true)
    val w = 'w of Vectors
    val model = LinearModel(feat, w)

    val instance = State(Map(r.atom(0) -> false, r.atom(1) -> true))

    val conditioned = model | instance

    val distConds = TermConverter.distConds(conditioned)
    val distDots = TermConverter.distDots(distConds)
    val unrolled = TermConverter.unrollLambdas(distDots)
    val flatten = TermConverter.flattenDouble(unrolled)
    println(conditioned)
    println(distConds)
    println(distDots)
    println(unrolled)
    println(flatten)

    val flatFG = build(flatten, w)
    val printer = new flatFG.FGPrinter(key)

    //    flatFG.fg.weights = new DenseVector(Array(1.0,2.0,3.0,4.0))
    flatFG.graph.weights = key.createDenseVector(
      Seq(false, false) -> 1.0,
      Seq(false, true) -> 2.0,
      Seq(true, false) -> 3.0,
      Seq(true, true) -> 4.0)()
    println(flatFG.graph.toVerboseString(printer))

    MaxProduct.run(flatFG.graph, 1)

    val feats = new SparseVector(100)
    MaxProduct.featureExpectationsAndObjective(flatFG.graph, feats)

    println(key.vectorToString(feats))

  }

}

