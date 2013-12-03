package scalapplcodefest

import cc.factorie.maths.ArrayOps
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scalapplcodefest.MPGraph.{Factor, Edge}

/**
 * Takes a term and builds a message passing graph for it.
 * This code is very preliminary and needs a complete rewrite.
 * @author Sebastian Riedel
 */
object MessagePassingGraphBuilder {

  import MPGraph._

  trait Recipe {
    def potential(fg: TermAlignedFG): StructuredPotential
  }
  val recipes = new ArrayBuffer[PartialFunction[Term[Double], Recipe]]

  class TermAlignedFG(val term: Term[Double], val weights: Variable[Vector]) {
    val vars = term.variables.toSeq.filter(_ != weights).sorted(VariableOrdering)
    val graph = new MPGraph
    def createVariableMapping(variable: Variable[Any]) = {
      val domain = variable.domain.eval().get.toSeq
      val indexOfValue = domain.zipWithIndex.toMap
      val node = graph.addNode(domain.size)
      VariableMapping(variable, node, domain, indexOfValue)
    }
    val variableMappings = vars.map(createVariableMapping)
    val variable2Mapping = variableMappings.map(m => m.variable -> m).toMap
    val dims = variableMappings.map(_.dom.size)
    val entryCount = dims.product
    lazy val node2variable = variableMappings.map(m => m.node -> m.variable).toMap
    val factor2Term = new mutable.HashMap[Factor, Term[Double]]

    def argmaxState() = {
      val map = for (v <- variableMappings) yield {
        val winner = ArrayOps.maxIndex(v.node.b)
        val value = v.dom(winner)
        v.variable -> value
      }
      State(map.toMap)
    }

    class FGPrinter(index: Index) extends MPGraph.FGPrinter {
      def node2String(node: Node) = node2variable(node).toString
      def factor2String(factor: Factor) = factor2Term(factor).toString
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
      val score = term.eval(state).get
      var stateIndex = 0
      for ((v, i) <- mappings.zipWithIndex) {
        val valueIndex = v.indexOfValue(state(v.variable))
        stateIndex = valueIndex + v.dom.size * stateIndex
        setting(i) = valueIndex
      }
      settings(stateIndex) = setting
      scores(stateIndex) = score
    }
    val f = aligned.graph.addTableFactor(scores, settings, dims)
    BuiltFactor(f, mappings)
  }

  def buildLinearFactor(aligned: TermAlignedFG, term: Term[Double], feats: Term[Vector],
                        base: Term[Double] = Constant(0.0),
                        condition: State = State.empty) = {
    val vars = term.variables.toSeq.filter(_ != aligned.weights).sorted(VariableOrdering)
    val mappings = vars.map(aligned.variable2Mapping)
    val dims = mappings.view.map(_.dom.size).toArray
    val entryCount = dims.view.product

    //create a factor using term and all variables
    val settings = Array.ofDim[Array[Int]](entryCount)
    val stats = Array.ofDim[Vector](entryCount)
    val baseScores = Array.ofDim[Double](entryCount)


    //iterate over possible states, get the state index
    for (state <- State.allStates(vars.toList)) {
      val setting = Array.ofDim[Int](vars.size)
      val feat = feats.eval(state + condition).get
      var stateIndex = 0
      for ((v, i) <- mappings.zipWithIndex) {
        val valueIndex = v.indexOfValue(state(v.variable))
        stateIndex = valueIndex + v.dom.size * stateIndex
        setting(i) = valueIndex
      }
      settings(stateIndex) = setting
      stats(stateIndex) = feat
      baseScores(stateIndex) = base.eval(state + condition).get
    }
    val f = aligned.graph.addLinearFactor(stats, baseScores, settings, dims)
    BuiltFactor(f, mappings)
  }


  def buildFactor(aligned: TermAlignedFG, term: Term[Double], weights: Variable[Vector]): BuiltFactor = {
    import TermImplicits._
    val result = term match {
      case s@Math.DoubleAdd.Reduced(SeqTerm(args)) =>
        val featsOrDoubles = args.map {
          case c@Math.Dot.Applied2(f, w) if w == weights => Right(f)
          case l => Left(l)
        }
        val feats = featsOrDoubles.collect({case Right(f) => f})
        val doubles = featsOrDoubles.collect({case Left(d) => d})
        val featSum = vsum(SeqTerm(feats))
        val doubleSum = dsum(SeqTerm(Constant(0.0) +: doubles))
        buildLinearFactor(aligned, s, featSum, doubleSum, State.empty)
      case l@Linear(feats, w, base) if w == weights => buildLinearFactor(aligned, l, feats, base)
      case t => buildTableFactor(aligned, t)
    }
    aligned.factor2Term(result.factor) = term
    result
  }

  object EdgeOrdering extends Ordering[(Node, Factor, Int)] {
    def compare(x: (Node, Factor, Int), y: (Node, Factor, Int)): Int = {
      val ((n1, f1, i1), (n2, f2, i2)) = (x, y)
      if (f1.rank != f2.rank) return f1.rank - f2.rank
      if (i1 != i2) return i2 - i1
      val sign = -1 + (i1 % 2) * 2 //edges can be forward processed or backward processed depending on their position in the factor
      if (n1.index != n2.index) return sign * (n1.index - n2.index)
      f1.index - f2.index
    }
  }

  def build(term: Term[Double], weights: Variable[Vector] = null): TermAlignedFG = {
    val aligned = new TermAlignedFG(term, weights)
    val edges = new ArrayBuffer[(Node, Factor, Int)]

    term match {
      case Reduce(ConstantFun(Math.DoubleAdd), SeqTerm(args)) =>
        for (arg <- args; if arg != Constant(0.0)) {
          val f = buildFactor(aligned, arg, weights)
          for ((mapping, index) <- f.vars.zipWithIndex) {
            edges += ((mapping.node, f.factor, index))
            //aligned.graph.addEdge(f.factor, mapping.node)
          }
        }
      case _ =>
        val f = buildFactor(aligned, term, weights)
        for ((mapping, index) <- f.vars.zipWithIndex) {
          edges += ((mapping.node, f.factor, index))
          //          aligned.graph.addEdge(f.factor, mapping.node)
        }
    }
    val sorted = edges.sorted(EdgeOrdering)
    for ((n, f, i) <- sorted)
      aligned.graph.addEdge(f, n, i)

    aligned.graph.build()
    aligned
  }

  def main(args: Array[String]) {

    recipes += {
      case Math.ExactlyOne.Applied(VarSeq(v)) => new Recipe {
        def potential(fg: TermAlignedFG) = new StructuredPotential {
          def maxScoreAndFeatures(factor: Factor, featureDest: scalapplcodefest.Vector) = ???
          def maxMarginal2Node(edge: Edge) = ???
          def maxMarginal2AllNodes(factor: Factor) = ???
          def score(factor: Factor, setting: Array[Int], weights: Vector) = ???
        }
      }
    }


  }

}

/**
 * Turns double terms into message passing factor graphs.
 */
object MPGraphCompiler {

  import MPGraph._
  import TermConverter._
  import Math._

  trait Recipe {
    def potential(term: Term[Any], nodes: Seq[MetaNode]): StructuredPotential
  }

  var dispatcher: PartialFunction[Term[Any], Recipe] = null

  case class MetaNode(variable: Variable[Any], node: Node, values: Seq[Any], indices: Map[Any, Int])
  case class MetaNodes(forNode: Map[Node, MetaNode], forVariable: Map[Variable[Any], MetaNode])

  case class Result(graph: MPGraph, meta: MetaNodes)

  /**
   * Flattens and unroll a term into a propositional factorization.
   * @param base the term to factorize.
   * @param op the binary operation to be used during factorization.
   * @tparam T type of term.
   * @return sequence of terms in the factorization. 
   */
  def factorize[T](base: Term[T], op: BinaryOperatorSameDomainAndRange[T]): Seq[Term[T]] = {
    val flat = flatten(base, op)
    val grouped = groupLambdas(flat)
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
    val factor = mpGraph.addLinearFactor(vectors, null, settings, dims)
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


