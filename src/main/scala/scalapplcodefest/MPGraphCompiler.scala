package scalapplcodefest

import cc.factorie.maths.ArrayOps

/**
 * Turns double terms into message passing factor graphs.
 *
 * @author Sebastian Riedel
 */
object MPGraphCompiler {

  import MPGraph._
  import TermConverter._
  import Math._

  /**
   * A recipe can generate a structured MPGraph potential for a given term and meta information about the nodes.
   */
  trait Recipe {
    def potential(term: Term[Any], nodes: Seq[MetaNode]): StructuredPotential
  }

  /**
   * A node with meta information on how it corresponding to a variable, and how the indices in
   * settings correspond to values.
   * @param variable the variable corresponding to the node.
   * @param node the node itself.
   * @param values the values of the variable, ordered as they are indiced in message passing.
   * @param indices a map from values to their indices.
   */
  case class MetaNode(variable: Variable[Any], node: Node, values: Seq[Any], indices: Map[Any, Int])

  /**
   * The collected meta information about variables and their alignments to nodes in the graph.
   * @param forNode find the meta node for a given node.
   * @param forVariable find the meta node for a given variable.
   */
  case class MetaNodes(forNode: Map[Node, MetaNode], forVariable: Map[Variable[Any], MetaNode])

  /**
   * The result of a compilation is the graph itself and meta information about how variables are aligned with nodes.
   * @param graph the built graph.
   * @param meta meta information.
   */
  case class Result(graph: MPGraph, meta: MetaNodes) {

    /**
     * Chooses a state by picking the maximizer of the node beliefs on each node.
     * @return a state based on maximizing the node beliefs.
     */
    def currentArgmax():State = {
      val map = for (m <- meta.forNode.values) yield {
        val winner = ArrayOps.maxIndex(m.node.b)
        val value = m.values(winner)
        m.variable -> value
      }
      State(map.toMap)
    }

    /**
     * Returns a state where beliefs of variables are stored, according to the current node beliefs.
     * @return a state in which each variable `x` of the term has a `Belief(x)` variable that is associated with
     *         a belief (function to doubles over values of the domain).
     */
    def currentBeliefs():State = {
      val map = for (m <- meta.forNode.values) yield {
        val beliefs = for ((value,belief) <- m.values.view zip m.node.b.view) yield value -> belief
        val fun = Fun(beliefs.toMap, m.variable.domain[Any].value(), Doubles)
        Belief(m.variable) -> fun
      }
      State(map.toMap)
    }

    /**
     * Returns the current gradient as stored in the graph.
     * @return current gradient.
     */
    def currentGradient():Vector = graph.gradient

    /**
     * Returns the current value as stored in the graph. This usually is some objective or expectation
     * message passing algorithms are calculating.
     * @return current value.
     */
    def currentValue():Double = graph.value


  }

  /**
   * Flattens and unroll a term into a propositional factorization.
   * @param base the term to factorize.
   * @param op the binary operation to be used during factorization.
   * @tparam T type of term.
   * @return sequence of terms in the factorization.
   */
  def factorize[T](base: Term[T], op: BinaryOperatorSameDomainAndRange[T]): Seq[Term[T]] = {
    val conditionsPushed = pushDownConditions(base)
    val flat = flatten(conditionsPushed, op)
    val dotsPushed = pushDownDotProducts(flat)
    val grouped = groupLambdas(dotsPushed)
    val brackets = bracketInsideLambda(grouped)
    val unrolled = unrollLambdaImages(brackets)
    val result = flatten(unrolled, op)
    val unbracketed = unbracket(result)
    asSeq(unbracketed, op)
  }

  /**
   * Create the nodes of the MP graph and remember their alignment to variables and their values.
   * @param vars the variables to create the nodes for.
   * @param graph the graph to create the nodes in.
   * @return a representation of the alignment from nodes and indices to variables and values.
   */
  def createNodes(vars: Seq[Variable[Any]], graph: MPGraph) = {
    val meta = for (v <- vars) yield {
      val dom = v.domain[Any].value().toArray
      val indices = dom.zipWithIndex.toMap
      val node = graph.addNode(dom.size)
      MetaNode(v, node, dom, indices)
    }
    val var2Meta = meta.map(m => m.variable -> m).toMap
    val node2Meta = meta.map(m => m.node -> m).toMap
    MetaNodes(node2Meta, var2Meta)
  }

  /**
   * Compiles a double term to a message passing factor graph.
   * @param term the term to turn into a message passing graph.
   * @return an MPGraph aligned to the terms, variables and values of the provided term.
   */
  def compile(term: Term[Double], dispatcher: PartialFunction[Term[Any], Recipe] = Map.empty): Result = {

    val ForceLinear(coefficient, _, base) = term
    val vars = (coefficient.variables ++ base.variables).toSeq
    val mpGraph = new MPGraph()
    val metaNodes = createNodes(vars, mpGraph)
    val factorizedBase = factorize(base, DoubleAdd)
    val factorizedCoefficient = factorize(coefficient, VecAdd)

    for (baseTerm <- factorizedBase) dispatcher.lift(baseTerm) match {
      case Some(recipe) => compileStructuredDouble(baseTerm, recipe, metaNodes, mpGraph)
      case None => compileBaseTerm(baseTerm, metaNodes, mpGraph)
    }

    for (coeffTerm <- factorizedCoefficient) dispatcher.lift(coeffTerm) match {
      case Some(recipe) => compileStructuredDouble(coeffTerm, recipe, metaNodes, mpGraph)
      case None => compileCoefficientTerm(coeffTerm, metaNodes, mpGraph)
    }
    mpGraph.build()
    Result(mpGraph, metaNodes)
  }

  /**
   * Creates a custom structured factor.
   * @param term the potential as term.
   * @param recipe recipe for creating the compiled potential.
   * @param metaNodes meta information about the graph.
   * @param mpGraph the graph to build.
   */
  def compileStructuredDouble(term: Term[Any], recipe: Recipe, metaNodes: MetaNodes, mpGraph: MPGraph) {
    val factorVars = term.variables.toList
    val factorMeta = factorVars.map(metaNodes.forVariable)
    val potential = recipe.potential(term, factorMeta)
    val factor = mpGraph.addStructuredFactor(potential)
    for ((m, i) <- factorMeta.zipWithIndex) mpGraph.addEdge(factor, m.node, i)
  }

  /**
   * Compile a single base term into a table factor.
   * @param baseTerm the base term to compile.
   * @param metaNodes information about how nodes and indices are aligned with variables and values.
   * @param mpGraph the graph to add to.
   */
  def compileBaseTerm(baseTerm: Term[Double], metaNodes: MPGraphCompiler.MetaNodes, mpGraph: MPGraph) {
    val factorVars = baseTerm.variables.toList
    val factorMeta = factorVars.map(metaNodes.forVariable)
    val dims = factorMeta.view.map(_.values.size).toArray
    val settingCount = dims.product
    val scores = Array.ofDim[Double](settingCount)
    val settings = processSettings(metaNodes, factorVars) {
      case (index, setting, state) =>
        val score = baseTerm.value(state)
        scores(index) = score
    }
    val factor = mpGraph.addTableFactor(scores, settings, dims)
    for ((m, i) <- factorMeta.zipWithIndex) mpGraph.addEdge(factor, m.node, i)
  }

  /**
   * Compile a single base term into a table factor.
   * @param baseTerm the base term to compile.
   * @param metaNodes information about how nodes and indices are aligned with variables and values.
   * @param mpGraph the graph to add to.
   */
  def compileCoefficientTerm(baseTerm: Term[Vector], metaNodes: MPGraphCompiler.MetaNodes, mpGraph: MPGraph) {
    val factorVars = baseTerm.variables.toList
    val factorMeta = factorVars.map(metaNodes.forVariable)
    val dims = factorMeta.view.map(_.values.size).toArray
    val settingCount = dims.product
    val vectors = Array.ofDim[Vector](settingCount)
    val settings = processSettings(metaNodes, factorVars) {
      case (index, setting, state) =>
        val vector = baseTerm.value(state)
        vectors(index) = vector
    }
    val factor = mpGraph.addLinearFactor(vectors, settings, dims)
    for ((m, i) <- factorMeta.zipWithIndex) mpGraph.addEdge(factor, m.node, i)
  }


  /**
   * Builds an array of settings, and while doing so, calls a method with side effects that
   * can create additional information on a per setting.
   * @param metaNodes meta information.
   * @param factorVars variables to create settings for.
   * @param process the processor to apply to each setting.
   * @return the array of settings.
   */
  def processSettings(metaNodes: MPGraphCompiler.MetaNodes,
                      factorVars: List[Variable[Any]])(process: (Int, Array[Int], State) => Unit): Array[Array[Int]] = {
    val factorMeta = factorVars.map(metaNodes.forVariable)
    val dims = factorMeta.view.map(_.values.size).toArray
    val settingCount = dims.product
    val settings = Array.ofDim[Array[Int]](settingCount)
    for (state <- State.allStates(factorVars)) {
      val setting = factorMeta.view.map(m => m.indices(state(m.variable))).toArray
      val settingIndex = MPGraph.settingToEntry(setting, dims)
      settings(settingIndex) = setting
      process(settingIndex, setting, state)
    }
    settings
  }


}
