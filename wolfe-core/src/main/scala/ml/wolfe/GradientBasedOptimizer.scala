package ml.wolfe

/**
 * @author Sebastian Riedel
 */
object GradientBasedOptimizer {

  def apply(fg:FactorGraph) {
    //initialize all n2f messages with zero or some random value
    //go over all factors, calculate gradients into f2n
    //collect all f2n messages into one gradient vector for factorie trainer
    //resample graph (hook in factorie?)

  }
}
