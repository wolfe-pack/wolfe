package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
object MaxProduct {

  import MPGraph._
  import MoreArrayOps._

  /**
   * Runs some iterations of belief propagation.
   * @param fg the message passing graph to run
   * @param maxIteration maximum number of iterations.
   * @param canonical should edges be processed in canonical ordering according to [[scalapplcodefest.MPGraph.EdgeOrdering]].
   */
  def apply(fg: MPGraph, maxIteration: Int, canonical:Boolean = true) {
    val edges = if (canonical) fg.edges.sorted(MPGraph.EdgeOrdering) else fg.edges

    for (i <- 0 until maxIteration) {
      for (edge <- edges) {
        for (other <- edge.f.edges; if other != edge) updateN2F(other)
        updateF2N(edge)
      }
    }
    for (node <- fg.nodes) updateBelief(node)

    //calculate gradient and objective
    //todo this is not needed if we don't have linear factors. Maybe initial size should depend on number of linear factors
    fg.gradient = new SparseVector(1000)
    fg.value = featureExpectationsAndObjective(fg,fg.gradient)

  }


  /**
   * Accumulates the expectations of all feature vectors under the current model. In MaxProduce expectations
   * are based on the MAP distribution.
   * @param fg factor graph.
   * @param result vector to add results to.
   */
  def featureExpectationsAndObjective(fg: MPGraph, result: Vector): Double = {
    var obj = 0.0
    for (factor <- fg.factors) {
      // 1) go over all states, find max
      var norm = Double.NegativeInfinity
      for (i <- 0 until factor.entryCount) {
        val setting = factor.settings(i)
        val score = penalizedScore(factor, i, setting)
        norm = math.max(score, norm)
      }

      obj += norm

      if (factor.typ == MPGraph.FactorType.LINEAR) {

        // 2) count number of maximums
        var maxCount = 0
        for (i <- 0 until factor.entryCount) {
          val setting = factor.settings(i)
          val score = penalizedScore(factor, i, setting)
          if (score == norm) maxCount += 1
        }

        // 3) prob = 1/|maxs| for all maximums, add corresponding vector
        for (i <- 0 until factor.entryCount) {
          val setting = factor.settings(i)
          val score = penalizedScore(factor, i, setting)
          if (score == norm) {
            result +=(factor.stats(i), 1.0 / maxCount)
          }
        }

      }
    }
    obj
  }

  /**
   * Calculates the score of a setting and adds penalties based on incoming messages of the factor.
   * @param factor the factor to calculate the penalised score for.
   * @param settingId id of the setting to score.
   * @param setting the setting corresponding to the id.
   * @return penalized score of setting.
   */
  def penalizedScore(factor: MPGraph.Factor, settingId: Int, setting: Array[Int]): Double = {
    var score = factor.score(settingId)
    for (j <- 0 until factor.rank) {
      score += factor.edges(j).n2f(setting(j))
    }
    score
  }

  /**
   * Updates the message from factor to node.
   * @param edge the factor-node edge.
   */
  def updateF2N(edge: Edge) {
    val factor = edge.f

    //remember last message for calculating residuals
    set(edge.f2n, edge.f2nLast)

    //initializing to low number for later maxing
    fill(edge.f2n, Double.NegativeInfinity)

    //max over all settings
    for (i <- 0 until factor.entryCount) {
      val setting = factor.settings(i)
      var score = factor.score(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- 0 until factor.rank; if j != edge.indexInFactor) {
        score += factor.edges(j).n2f(setting(j))
      }
      edge.f2n(varValue) = math.max(score, edge.f2n(varValue))
    }

    //normalizing by max value
    maxNormalize(edge.f2n)
  }

  /**
   * Updates the message from a node to a factor.
   * @param edge the factor-node edge.
   */
  def updateN2F(edge: Edge) {
    val node = edge.n
    System.arraycopy(node.in, 0, edge.n2f, 0, edge.n2f.length)
    for (i <- 0 until node.dim) {
      for (e <- 0 until node.edges.length; if e != edge.indexInNode)
        edge.n2f(i) += node.edges(e).f2n(i)
    }
  }

  /**
   * Updates the belief (sum of incoming messages) at a node.
   * @param node the node to update.
   */
  def updateBelief(node: Node) {
    System.arraycopy(node.in, 0, node.b, 0, node.b.length)
    for (e <- 0 until node.edges.length)
      for (i <- 0 until node.dim)
        node.b(i) += node.edges(e).f2n(i)
  }





}







