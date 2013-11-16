package scalapplcodefest

import scalaxy.loops._
import java.util
import cc.factorie.maths.ArrayOps


/**
 * @author Sebastian Riedel
 */
object MaxProduct {

  import FG._

  def main(args: Array[String]) {
    val fg = new FG
    val f1 = fg.createFactor2(Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)))
    val n1 = fg.createNode(2)
    val n2 = fg.createNode(3)
    val e1 = fg.createEdge(f1, n1)
    val e2 = fg.createEdge(f1, n2)
    fg.build()

    MaxProduct.run(fg, 1)

    println(n1.b.mkString(" "))
    println(n2.b.mkString(" "))

  }

  def run(fg: FG, maxIteration: Int) {
    for (i <- (0 until maxIteration).optimized) {
      for (edge <- fg.edges) {
        updateN2F(edge)
        updateF2N(edge)
      }
    }
    for (node <- fg.nodes) updateBelief(node)
  }

  def updateF2N(edge: Edge) {
    val factor = edge.f
    util.Arrays.fill(edge.f2n, Double.MinValue)
    for (i <- 0 until factor.entryCount) {
      val setting = factor.settings(i)
      var score = factor.score(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- 0 until factor.rank; if j != edge.indexInFactor) {
        score += factor.edges(j).n2f(setting(j))
      }
      edge.f2n(varValue) = math.max(score, edge.f2n(varValue))
    }
  }

  def updateN2F(edge: Edge) {
    val node = edge.n
    System.arraycopy(node.in, 0, edge.n2f, 0, edge.n2f.length)
    for (i <- 0 until node.dim) {
      for (e <- 0 until node.edges.length; if e != edge.indexInNode)
        edge.n2f(i) += node.edges(e).f2n(i)
    }
  }

  def updateBelief(node: Node) {
    System.arraycopy(node.in, 0, node.b, 0, node.b.length)
    for (e <- 0 until node.edges.length)
      for (i <- 0 until node.dim)
        node.b(i) += node.edges(e).f2n(i)
  }
}



object FGBuilder {

  import FG._

  class TermAlignedFG(val term:Term[Double], val weights:Variable[Vector]) {
    val vars = term.variables.toSeq.filter(_ != weights)
    val fg = new FG
    def createVariableMapping(variable:Variable[Any]) = {
      val domain = variable.domain.eval().get.toSeq
      val indexOfValue = domain.zipWithIndex.toMap
      val node = fg.createNode(domain.size)
      VariableMapping(variable, node, domain, indexOfValue)
    }
    val variableMappings = vars.map(createVariableMapping)
    val variable2Mapping = variableMappings.map(m => m.variable -> m).toMap
    val dims = variableMappings.map(_.dom.size)
    val entryCount = dims.product

    def beliefToState() = {
      val map = for (v <- variableMappings) yield {
        val winner = ArrayOps.maxIndex(v.node.b)
        val value = v.dom(winner)
        v.variable -> value
      }
      State(map.toMap)
    }
  }

  case class VariableMapping(variable:Variable[Any],node:Node, dom:Seq[Any], indexOfValue:Map[Any,Int])

  def main(args: Array[String]) {
    import TermImplicits._
    val r = 'r of (0 ~~ 2 |-> Bools)
    val s = 's of (0 ~~ 2 |-> Bools)

    val f = dsum(for (i <- 0 ~~ 1) yield I(!(r(i) || s(i))))

    println(f.eval(r.atom(0) -> true, s.atom(0) -> false))

    val fg = build(f)

    MaxProduct.run(fg.fg,1)
    println(fg.beliefToState().toPrettyString)

    val key = new Index
    val feat = vsum(for (i <- 0 ~~ 2) yield e_(key(r(i),s(i))))

    val vec = feat.eval(r.atom(0) -> true, s.atom(0) -> false, r.atom(1) -> false, s.atom(1) -> true)
    val w = 'w of Vectors
    val model = LinearModel(feat,w)

    println(vec.map(_.mkString(" ")))

    val fg2 = build(model,w)
    fg2.fg.weights = new DenseVector(Array(1.0,2.0,3.0,4.0))

    println(fg2.fg.toVerboseString(key))
    println(key.vectorToString(fg2.fg.weights, "\n"))





  }


  def buildTableFactor(aligned:TermAlignedFG,term:Term[Double]) = {
    val vars = term.variables.toSeq.filter(_ != aligned.weights)
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
      for ((v,i) <- aligned.variableMappings.zipWithIndex) {
        val valueIndex = v.indexOfValue(state(v.variable))
        stateIndex = valueIndex + v.dom.size * stateIndex
        setting(i) = valueIndex
      }
      settings(stateIndex) = setting
      scores(stateIndex) = score
    }
    val f = aligned.fg.createFactor(scores, settings, dims)
    f
  }

  def buildLinearFactor(aligned:TermAlignedFG, term:LinearModel):FG.Factor = {
    val vars = term.variables.toSeq.filter(_ != aligned.weights)
    val mappings = vars.map(aligned.variable2Mapping)
    val dims = mappings.view.map(_.dom.size).toArray
    val entryCount = dims.view.product

    //create a factor using term and all variables
    val settings = Array.ofDim[Array[Int]](entryCount)
    val stats = Array.ofDim[SparseVector](entryCount)

    //iterate over possible states, get the state index
    for (state <- State.allStates(vars.toList)) {
      val setting = Array.ofDim[Int](vars.size)
      val feat = term.features.eval(state).get.asInstanceOf[SparseVector] //todo: maybe be more generic here
      var stateIndex = 0
      for ((v,i) <- aligned.variableMappings.zipWithIndex) {
        val valueIndex = v.indexOfValue(state(v.variable))
        stateIndex = valueIndex + v.dom.size * stateIndex
        setting(i) = valueIndex
      }
      settings(stateIndex) = setting
      stats(stateIndex) = feat
    }
    val f = aligned.fg.createLinearFactor(stats, settings, dims)
    f
  }

  def buildFactor(aligned:TermAlignedFG, term:Term[Double], weights:Variable[Vector]):FG.Factor = {
    term match {
      case l@LinearModel(feats,w,base) if w == weights => buildLinearFactor(aligned, l)
      case t => buildTableFactor(aligned,t)
    }
  }

  def build(term: Term[Double], weights:Variable[Vector] = null): TermAlignedFG = {
    val aligned = new TermAlignedFG(term,weights)

    val f = buildFactor(aligned,term,weights)
    for (n <- aligned.fg.nodes) {
      aligned.fg.createEdge(f, n)
    }

    aligned.fg.build()
    aligned
  }
}


