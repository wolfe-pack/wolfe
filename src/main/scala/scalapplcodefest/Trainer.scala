package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
object Trainer {

  def train(model: LinearModel, instances: Seq[State]): Vector = {
    import TermImplicits._
    val weights = new DenseVector(10000)
    for (instance <- instances) {
      val conditioned = model | instance

      val aligned = MessagePassingGraphBuilder.build(conditioned,model.weights)
      aligned.graph.weights = weights
      MaxProduct.run(aligned.graph, 1)


      val guessFeats = MaxProduct.featureExpectations(aligned.graph)
      val obj = MaxProduct.objective(aligned.graph)


    }
    //have: MaxProduct.
    //need:
    ???
  }


}
