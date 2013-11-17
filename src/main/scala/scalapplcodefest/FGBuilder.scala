package scalapplcodefest

import cc.factorie.maths.ArrayOps

/**
 * @author Sebastian Riedel
 */
object FGBuilder {

  import FG._

  class TermAlignedFG(val term:Term[Double], val weights:Variable[Vector]) {
    val vars = term.variables.toSeq.filter(_ != weights)
    val fg = new FG
    def createVariableMapping(variable:Variable[Any]) = {
      val domain = variable.domain.eval().get.toSeq
      val indexOfValue = domain.zipWithIndex.toMap
      val node = fg.addNode(domain.size)
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


  case class BuiltFactor(factor:Factor,vars:Seq[VariableMapping])

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
    val f = aligned.fg.addFactor(scores, settings, dims)
    BuiltFactor(f,mappings)
  }

  def buildLinearFactor(aligned:TermAlignedFG, term:LinearModel) = {
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
    val f = aligned.fg.addLinearFactor(stats, settings, dims)
    BuiltFactor(f,mappings)
  }


  def buildFactor(aligned:TermAlignedFG, term:Term[Double], weights:Variable[Vector]):BuiltFactor = {
    term match {
      case l@LinearModel(feats,w,base) if w == weights => buildLinearFactor(aligned, l)
      case t => buildTableFactor(aligned,t)
    }
  }

  def build(term: Term[Double], weights:Variable[Vector] = null): TermAlignedFG = {
    val aligned = new TermAlignedFG(term,weights)

    term match {
      case Reduce(ConstantFun(Math.DoubleAdd),SeqTerm(args)) =>
        for (arg <- args) {
          val f = buildFactor(aligned,arg,weights)
          for (mapping <- f.vars){
            aligned.fg.addEdge(f.factor, mapping.node)
          }
        }
      case _ =>
        val f = buildFactor(aligned,term,weights)
        for (mapping <- f.vars){
          aligned.fg.addEdge(f.factor, mapping.node)
        }
    }

    aligned.fg.build()
    aligned
  }

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

    val distributed = TermConverter.distDots(model)
    val unrolled = TermConverter.unrollLambdas(distributed)
    val flatten = TermConverter.flattenDouble(unrolled)
    println(distributed)
    println(unrolled)
    println(flatten)

  }

}

