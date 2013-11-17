package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
object Trainer {

  def train(model: LinearModel, instances: Seq[State]): Vec = {
    import TermImplicits._
    val weights = new DenseVec(100)
    for (instance <- instances) {
      val conditioned = model | instance
      //todo: this could also allow specification of a weight variable that the builder will spot
      //todo: in LogLinear terms. The build fg will then provide the possibility to set this weight directly
      //todo: and to read off the feature expectations
      val aligned = MessagePassingGraphBuilder.build(conditioned)
      MaxProduct.run(aligned.fg, 1)

    }
    //have: MaxProduct.
    //need:
    ???
  }


}
