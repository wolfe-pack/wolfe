package scalapplcodefest

import cc.factorie.maths.ArrayOps
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
    val factor2Term = new mutable.HashMap[Factor,Term[Double]]

    def argmaxState() = {
      val map = for (v <- variableMappings) yield {
        val winner = ArrayOps.maxIndex(v.node.b)
        val value = v.dom(winner)
        v.variable -> value
      }
      State(map.toMap)
    }

    class FGPrinter(index:Index) extends MessagePassingFactorGraph.FGPrinter {
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
    val f = aligned.graph.addTableFactor(scores, settings, dims)
    BuiltFactor(f, mappings)
  }

  def buildLinearFactor(aligned: TermAlignedFG, term: Term[Double], feats: Term[Vector],
                        base:Term[Double] = Constant(0.0),
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
      val feat = feats.eval(state + condition).right.get
      var stateIndex = 0
      for ((v, i) <- mappings.zipWithIndex) {
        val valueIndex = v.indexOfValue(state(v.variable))
        stateIndex = valueIndex + v.dom.size * stateIndex
        setting(i) = valueIndex
      }
      settings(stateIndex) = setting
      stats(stateIndex) = feat
      baseScores(stateIndex) = base.eval(state + condition).right.get
    }
    val f = aligned.graph.addLinearFactor(stats, baseScores, settings, dims)
    BuiltFactor(f, mappings)
  }


  def buildFactor(aligned: TermAlignedFG, term: Term[Double], weights: Variable[Vector]): BuiltFactor = {
    import TermImplicits._
    val result = term match {
      case l@Linear(feats, w, base) if w == weights => buildLinearFactor(aligned, l, feats, base)
      case c@Conditioned(Linear(feats, w, base), cond) if w == weights => buildLinearFactor(aligned, c, feats, base, cond)
      case s@Conditioned(Math.DoubleAdd.Reduced(SeqTerm(args)),outerCondition) =>
        val featsOrDoubles = args.map {
          case c@Math.Dot.Applied2(f, w) if w == weights => Right(f)
          case l => Left(l)
        }
        val feats = featsOrDoubles.collect({case Right(f) => f})
        val doubles = featsOrDoubles.collect({case Left(d) => d})
        val featSum = vsum(SeqTerm(feats))
        val doubleSum = dsum(SeqTerm(Constant(0.0) +: doubles))
        //todo: do something with the non-linear terms
        buildLinearFactor(aligned,s,featSum,doubleSum,outerCondition)
      case t => buildTableFactor(aligned, t)
    }
    aligned.factor2Term(result.factor) = term
    result
  }

  object EdgeOrdering extends Ordering[(Node,Factor,Int)] {
    def compare(x: (Node, Factor,Int), y: (Node, Factor,Int)):Int = {
      val ((n1,f1,i1),(n2,f2,i2)) = (x,y)
      if (f1.rank != f2.rank) return f1.rank - f2.rank
      if (i1 != i2) return i2 - i1
      val sign = -1 + (i1 % 2) * 2 //edges can be forward processed or backward processed depending on their position in the factor
      if (n1.index != n2.index) return sign * (n1.index - n2.index)
      f1.index - f2.index
    }
  }

  def build(term: Term[Double], weights: Variable[Vector] = null): TermAlignedFG = {
    val aligned = new TermAlignedFG(term, weights)
    val edges = new ArrayBuffer[(Node,Factor, Int)]

    term match {
      case Reduce(ConstantFun(Math.DoubleAdd), SeqTerm(args)) =>
        for (arg <- args; if arg != Constant(0.0)) {
          val f = buildFactor(aligned, arg, weights)
          for ((mapping,index) <- f.vars.zipWithIndex) {
            edges += ((mapping.node,f.factor,index))
            //aligned.graph.addEdge(f.factor, mapping.node)
          }
        }
      case _ =>
        val f = buildFactor(aligned, term, weights)
        for ((mapping,index) <- f.vars.zipWithIndex) {
          edges += ((mapping.node,f.factor,index))
//          aligned.graph.addEdge(f.factor, mapping.node)
        }
    }
    val sorted = edges.sorted(EdgeOrdering)
    for ((n,f,i) <- sorted)
      aligned.graph.addEdge(f,n,i)

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
    println(fg.argmaxState().toPrettyString)

    val key = new Index
    val feat = vsum(for (i <- 0 ~~ 2) yield unit(key(r(i), s(i))))

    val vec = feat.eval(r.atom(0) -> true, s.atom(0) -> false, r.atom(1) -> false, s.atom(1) -> true)
    val w = 'w of Vectors
    val model = LinearModel(feat, w)

    val instance = State(Map(r.atom(0) -> false, r.atom(1) -> true))

    val conditioned = model | instance

    val distConds = TermConverter.distConds(conditioned)
    val distDots = TermConverter.distDots(distConds)
    val unrolled = TermConverter.unrollLambdas(distDots)
    val flatten = TermConverter.flatten(unrolled,Math.DoubleAdd)
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

