package ml.wolfe.fg20

import ml.wolfe._

/**
 * A Potential is a function from settings of variables to real values. Many Machine Learning tasks can
 * be cast as either maximizing or summing over a set of variables with respect to an objective function, which
 * itself is a sum of such potential functions. To support these operations efficiently, potentials can provide additional
 * methods beyond the actual setting-to-value function. For example, to support gradient-based optimization
 * potentials that mix-in [[ml.wolfe.fg20.GradientBasedOptimizer.Potential]] need to provide their
 * gradients at a given parameter setting.
 *
 * Potentials can have three types of variables: discrete, continuous and vector variables. The potential provides these
 * in arrays, one for each type. The order of variables in these arrays is important. All settings and partial settings
 * that will be passed to the potential for computation will be consistent with this order, in the sense that the
 * i-th value in the discrete/continuous/vector setting array is the value assigned to the
 * i-th value in the discrete/continuous/vector variable array.
 *
 * The same potentials (as part of the same [[ml.wolfe.fg20.Problem]]) can be shared by several algorithms
 * at the same time. This requires that when a potential wants to maintain/cache a state for efficient
 * processing of computations such as gradients, it needs to be able to spawn new processors for each algorithm.
 * This is achieved by the [[ml.wolfe.fg20.Potential#processor]] method which returns a [[ml.wolfe.fg20.Processor]]
 * object dedicated to the algorithm. It's the processor which does all the work. This can creates
 * some overhead for Potentials that can do stateless computation, and hence there is a [[ml.wolfe.fg20.StatelessComputation]]
 * trait to simplify this case.
 *
 * @author Sebastian Riedel
 */
trait Potential {

  /**
   * @return an array of the discrete variables that this potential maintains.
   */
  def discVars: Array[DiscVar[Any]]

  /**
   * @return an array of the continuous variables that this potential maintains.
   */
  def contVars: Array[ContVar]

  /**
   * @return an array of the vector variables that this potential maintains.
   */
  def vectVars: Array[VectVar]

  /**
   * The type of processor this potential spawns.
   */
  type Proc <: Processor

  /**
   * Spawn a new processor in charge of doing computation for this potential.
   * @return a new processor (or an existing but stateless processor).
   */
  def processor(): Proc

  /**
   * Convenience method to create setting objects corresponding to the signature of the potential.
   * @return a setting that has as many discrete, continuous and vector settings as the potential has corresponding
   *         variables.
   */
  def createSetting() = new Setting(discVars.size, contVars.size, vectVars.size)

}

/**
 * A Processor is in charge of calculating the computations necessary for working with a potential.
 * Specific sub-traits of this trait can require more types of calculation. For example,
 * [[ml.wolfe.fg20.MaxProduct.Processor]] requires the processor to calculate outgoing messages, and
 * [[ml.wolfe.fg20.GradientBasedOptimizer.Processor]] requires the processor to calculate gradients.
 */
trait Processor {
  /**
   * Calculates the value of the potential at a given setting.
   * @param setting a setting to the discrete, continuous and vector variables of a potential.
   * @return the score the potential should return.
   */
  def score(setting: Setting): Double
}

/**
 * A convenience interface for potentials that do stateless computation. Here the potential itself
 * can do all the computation, and be reused within several algorithms at the same time.
 * @tparam This A self type of the class that implements this trait.
 */
trait StatelessComputation[This <: StatelessComputation[This]] extends Potential with Processor {
  this: This =>
  type Proc = This
  def processor(): This = this
}

/**
 * Some potentials use statistics/feature vectors to calculate their score. They mix in this trait.
 */
trait Statistics {
  /**
   * A feature representation of the provided state.
   * @param setting the setting to extract features from.
   * @return a feature representation of the state.
   */
  def stats(setting: Setting): FactorieVector
}

/**
 * A processor for an exponential family potential that uses a dot product of feature representation
 * and weight vector to calculate its value. The weight vector should be provided as the last
 * vector variable argument of the potential.
 */
trait ExpFamProcessor extends Statistics with Processor {

  /**
   * By convention the weight vector variable is the last vector variable of
   * the potential. This method implements this convention.
   * @param setting a setting to the variables of the potential.
   * @return the weight vector setting.
   */
  def weights(setting: Setting) = setting.vect(setting.vect.length - 1)

  /**
   * This method returns the dot product of weights and statistics.
   * @param setting a setting to the discrete, continuous and vector variables of a potential.
   * @return the score the potential should return.
   */
  def score(setting: Setting) = weights(setting) dot stats(setting)
}

/**
 * An exponential family potential.
 */
trait ExpFamPotential extends Potential {type Proc <: ExpFamProcessor }


/**
 * Convenience trait for potentials that only connect discrete variables.
 */
trait DiscPotential extends Potential {
  val contVars: Array[ContVar] = Array.ofDim[ContVar](0)
  val vectVars: Array[VectVar] = Array.ofDim[VectVar](0)
}

/**
 * Convenience trait for potentials that only connect continuous variables.
 */
trait ContPotential extends Potential {
  val discVars: Array[DiscVar[Any]] = Array.ofDim[DiscVar[Any]](0)
  val vectVars: Array[VectVar]      = Array.ofDim[VectVar](0)
}

/**
 * Convenience trait for potentials that only connect vector variables.
 */
trait VectPotential extends Potential {
  val discVars: Array[DiscVar[Any]] = Array.ofDim[DiscVar[Any]](0)
  val contVars: Array[ContVar]      = Array.ofDim[ContVar](0)
}

