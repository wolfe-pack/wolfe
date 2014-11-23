package ml.wolfe.fg20

import ml.wolfe.MoreArrayOps._
import ml.wolfe.SparseVector

trait FactorSettings extends FactorGraph {

  class FactorType(val pot: Pot) extends Factor {
    var setting = new Setting(pot.discVars.length, pot.contVars.length, pot.vectVars.length)
  }

  def syncFactorObservations(): Unit = {
    for (n <- discNodes; if n.observed; e <- n.edges) e.factor.setting.disc(e.index) = n.setting
    for (c <- contNodes; if c.observed; e <- c.edges) e.factor.setting.cont(e.index) = c.setting
    for (v <- vectNodes; if v.observed; e <- v.edges) e.factor.setting.vect(e.index) = v.setting

  }


}

class BruteForce(val problem: Problem) extends NodeContentFactorGraph with EmptyEdgeFactorGraph with FactorSettings {



  def createFactor(pot: Pot) = new FactorType(pot)

  type NodeContent = Any
  class ContNodeContent
  class VectNodeContent
  class DiscNodeContent(var belief: Array[Double])
  def createDiscNodeContent(variable: DiscVar[Any]) = new DiscNodeContent(Array.ofDim[Double](variable.dom.size))
  def createContNodeContent(contVar: ContVar) = new ContNodeContent
  def createVectNodeContent(vectVar: VectVar) = new VectNodeContent
  def acceptPotential = { case p: Potential => p }
  type Pot = Potential


  val expFamStats = factors.map(f => f -> f.processor).collect({ case (f, s: Statistics) => (f, s) })

  def loopOverSettings(nodes: List[DiscNode], loop: (() => Unit) => Unit = body => body()): (() => Unit) => Unit = {
    nodes match {
      case Nil => (body: () => Unit) => loop(body)
      case head :: tail =>
        def newLoop(body: () => Unit) {
          if (!head.observed) for (setting <- head.variable.dom.indices) {
            for (edge <- head.edges) edge.factor.setting.disc(edge.index) = setting
            head.setting = setting
            loop(body)
          } else loop(body)
        }
        loopOverSettings(tail, newLoop)
    }
  }

  def currentScore() = {
    factors.iterator.map(f => f.processor.score(f.setting)).sum
  }


  def inferMAP(): MAPResult = {

    //initialize observations in factor settings
    syncFactorObservations()

    val nodes = var2DiscNode.values.toList
    val loop = loopOverSettings(nodes)
    var maxScore = Double.NegativeInfinity
    for (n <- nodes) fill(n.content.belief, Double.NegativeInfinity)

    var maxSetting: Array[Int] = null
    loop { () =>
      val score = currentScore()
      for (n <- nodes) {
        n.content.belief(n.setting) = math.max(score, n.content.belief(n.setting))
      }

      if (score > maxScore) {
        maxScore = score
        maxSetting = nodes.view.map(_.setting).toArray
      }
    }

    for ((s, n) <- maxSetting zip nodes; if !n.observed) {
      n.setting = s
      maxNormalize(n.content.belief)
      for (e <- n.edges) e.factor.setting.disc(e.index) = s
    }

    val gradient = new SparseVector(1000)
    for ((f, s) <- expFamStats)
      gradient += s.stats(f.setting)

    val state = nodes.map(n => n.variable -> n.variable.dom(n.setting))
    val maxMarginals = nodes.map(n =>
      DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief.map(math.exp)))
    MAPResult(new MapBasedState(state.toMap), maxScore, gradient, new MapBasedState(maxMarginals.toMap))
  }

  def inferMarginals(): MarginalResult = {
    syncFactorObservations()

    val nodes = var2DiscNode.values.toList
    val loop = loopOverSettings(nodes)
    var Z = 0.0
    for (n <- nodes) fill(n.content.belief, 0.0)

    loop { () =>
      val score = currentScore()
      val prob = math.exp(score)
      for (n <- nodes) {
        n.content.belief(n.setting) += prob
      }
      Z += prob
    }

    for (n <- nodes) {
      normalize(n.content.belief)
      log(n.content.belief)
    }

    val logZ = math.log(Z)
    val gradient = new SparseVector(1000)

    loop { () =>
      val score = currentScore()
      val prob = math.exp(score) / Z
      for ((f, s) <- expFamStats)
        gradient +=(s.stats(f.setting), prob)
    }

    val marginals = nodes.map(n =>
      DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief.map(math.exp)))
    MarginalResult(logZ, gradient, new MapBasedState(marginals.toMap))
  }


}

