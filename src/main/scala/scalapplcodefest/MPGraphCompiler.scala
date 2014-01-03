package scalapplcodefest

import cc.factorie.maths.ArrayOps
import scalapplcodefest.value.{Fun, Doubles}
import TermDSL._
import scalapplcodefest.term._
import scalapplcodefest.term.Belief
import scala.Some

/**
 * Turns double terms into message passing factor graphs.
 *
 * @author Sebastian Riedel
 */
object MPGraphCompiler {

  import MPGraph._
  import TermConverter._

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
  case class MetaNodeInfo(forNode: Map[Node, MetaNode], forVariable: Map[Variable[Any], MetaNode]) {
    def variables(term: Term[Any]) = term.variables.filter(forVariable.isDefinedAt)
  }

  /**
   * Information about the mapping from factors to term.
   * @param forCoeff get the term for a coeff factor.
   * @param forBase get the term for a base factor.
   */
  case class MetaFactorInfo(forCoeff: Map[Factor, Term[Vector]], forBase: Map[Factor, Term[Double]])

  /**
   * The result of a compilation is the graph itself and meta information about how variables are aligned with nodes.
   * @param graph the built graph.
   * @param metaNodes meta information about nodes
   * @param metaFactors meta information about factors.
   */
  case class Result[T](sig: Sig[T], graph: MPGraph, metaNodes: MetaNodeInfo, metaFactors: MetaFactorInfo) {

    /**
     * Chooses a state by picking the maximizer of the node beliefs on each node.
     * @return a state based on maximizing the node beliefs.
     */
    def currentArgmaxState(): State = {
      val map = for (m <- metaNodes.forNode.values) yield {
        val winner = ArrayOps.maxIndex(m.node.b)
        val value = m.values(winner)
        m.variable -> value
      }
      State(map.toMap)
    }

    def currentArgmax() = sig.value(currentArgmaxState())

    /**
     * Returns a state where beliefs of variables are stored, according to the current node beliefs.
     * @return a state in which each variable `x` of the term has a `Belief(x)` variable that is associated with
     *         a belief (function to doubles over values of the domain).
     */
    def currentBeliefs(): State = {
      val map = for (m <- metaNodes.forNode.values) yield {
        val beliefs = for ((value, belief) <- m.values.view zip m.node.b.view) yield value -> belief
        val fun = Fun(beliefs.toMap, m.variable.domain[Any].value(), Doubles)
        Belief(m.variable) -> fun
      }
      State(map.toMap)
    }

    /**
     * Returns the current gradient as stored in the graph.
     * @return current gradient.
     */
    def currentGradient(): Vector = graph.gradient

    /**
     * Returns the current value as stored in the graph. This usually is some objective or expectation
     * message passing algorithms are calculating.
     * @return current value.
     */
    def currentValue(): Double = graph.value

    /**
     * Creates a custom printer for the factor graph in this result.
     * @param index an index to use when printing the feature vectors.
     * @return a printer for a factor graph.
     */
    def printer(index: Option[Index] = Index.toDebug) = new MPGraph.FGPrinter {
      def node2String(node: Node) = metaNodes.forNode(node).variable.toString
      def factor2String(factor: Factor) = metaFactors.forCoeff.get(factor).orElse(metaFactors.forBase.get(factor))
        .get.toString
      def vector2String(vector: scalapplcodefest.Vector) = index.map(_.vectorToString(vector,
        " ")).getOrElse(vector.toString())
    }

    /**
     * Prints out the resulting graph.
     * @return a string representing the factor graph.
     */
    override def toString = graph.toVerboseString(printer())
  }

  //rockt: I feel this method is not MPGraph specific and could be placed to TermConverter
  /**
   * Flattens and unroll a term into a propositional factorization.
   * @param base the term to factorize.
   * @param op the binary operation to be used during factorization.
   * @tparam T type of term.
   * @return sequence of terms in the factorization.
   */
  def factorize[T](base: Term[T], op: ConstantOperator[T]): Seq[Term[T]] = {
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
    MetaNodeInfo(node2Meta, var2Meta)
  }

  /**
   * Compiles a double term to a message passing factor graph.
   * @param term the term to turn into a message passing graph.
   * @return an MPGraph aligned to the terms, variables and values of the provided term.
   */
  def compile[T](sig: Sig[T], term: Term[Double],
                 dispatcher: PartialFunction[Term[Any], Recipe] = Map.empty): Result[T] = {

    val ForceLinear(coefficient, _, base) = term

    // hack alert!
    val isZero = coefficient match {
      case Constant(z) => true
      case _ => false
    }

    val sigVars = sig.variables
    val vars = (coefficient.variables ++ base.variables).filter(sigVars).toSeq.sorted(VariableOrdering)
    val mpGraph = new MPGraph()
    val metaNodes = createNodes(vars, mpGraph)
    val factorizedBase = factorize(base, doubles.add).toArray
    val factorizedCoefficient = factorize(coefficient, vectors.add).toArray

    val baseFactors = for (baseTerm <- factorizedBase) yield dispatcher.lift(baseTerm) match {
      case Some(recipe) => compileStructuredDouble(baseTerm, recipe, metaNodes, mpGraph) -> baseTerm
      case None => compileBaseTerm(baseTerm, metaNodes, mpGraph) -> baseTerm
    }

    val coeffFactors =
      if (isZero)
        new Array[(Factor, Term[Vector])](0)
      else for (coeffTerm <- factorizedCoefficient) yield dispatcher.lift(coeffTerm) match {
        case Some(recipe) => compileStructuredDouble(coeffTerm, recipe, metaNodes, mpGraph) -> coeffTerm
        case None => compileCoefficientTerm(coeffTerm, metaNodes, mpGraph) -> coeffTerm
      }
    mpGraph.build()
    Result(sig, mpGraph, metaNodes, MetaFactorInfo(coeffFactors.toMap, baseFactors.toMap))
  }

  /**
   * Creates a custom structured factor.
   * @param term the potential as term.
   * @param recipe recipe for creating the compiled potential.
   * @param metaNodes meta information about the graph.
   * @param mpGraph the graph to build.
   */
  def compileStructuredDouble(term: Term[Any], recipe: Recipe, metaNodes: MetaNodeInfo, mpGraph: MPGraph): Factor = {
    val factorVars = metaNodes.variables(term).toList.sorted(VariableOrdering)
    val factorMeta = factorVars.map(metaNodes.forVariable)
    val potential = recipe.potential(term, factorMeta)
    val factor = mpGraph.addStructuredFactor(potential)
    for ((m, i) <- factorMeta.zipWithIndex) mpGraph.addEdge(factor, m.node, i)
    factor
  }

  /**
   * Compile a single base term into a table factor.
   * @param baseTerm the base term to compile.
   * @param metaNodes information about how nodes and indices are aligned with variables and values.
   * @param mpGraph the graph to add to.
   */
  def compileBaseTerm(baseTerm: Term[Double], metaNodes: MPGraphCompiler.MetaNodeInfo, mpGraph: MPGraph): Factor = {
    val factorVars = metaNodes.variables(baseTerm).toList.sorted(VariableOrdering)
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
    factor
  }

  /**
   * Compile a single base term into a table factor.
   * @param baseTerm the base term to compile.
   * @param metaNodes information about how nodes and indices are aligned with variables and values.
   * @param mpGraph the graph to add to.
   */
  def compileCoefficientTerm(baseTerm: Term[Vector], metaNodes: MPGraphCompiler.MetaNodeInfo,
                             mpGraph: MPGraph): Factor = {
    val factorVars = metaNodes.variables(baseTerm).toList.sorted(VariableOrdering)
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
    factor
  }


  /**
   * Builds an array of settings, and while doing so, calls a method with side effects that
   * can create additional information on a per setting.
   * @param metaNodes meta information.
   * @param factorVars variables to create settings for.
   * @param process the processor to apply to each setting.
   * @return the array of settings.
   */
  def processSettings(metaNodes: MPGraphCompiler.MetaNodeInfo,
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
