package ml.wolfe.fg20

import ml.wolfe._

/**
 * A Potential is a function from settings of variables to real values. Many Machine Learning tasks can
 * be cast as either maximizing or summing over a set of variables with respect to an objective function, which
 * itself is a sum of such potential functions. To support these operations efficiently, potentials can provide additional
 * methods beyond the actual setting-to-value function. For example, to support gradient-based optimization
 * potentials that mix-in [[ml.wolfe.fg20.Differentiable]] need to provide their
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
 * This is achieved by methods that returns a processors such as the score method that
 * returns a [[ml.wolfe.fg20.Scorer]] dedicated to return the potential scores given a setting.
 *
 * @author Sebastian Riedel
 */
trait Potential extends Clique {


  /**
   * Spawn a new processor in charge of implementing the potential scoring function.
   * @return a new scorer (or an existing but stateless scorer).
   */
  def scorer(): Scorer

}

trait Clique {

  /**
   * @return an array of the discrete variables of this clique.
   */
  def discVars: Array[DiscVar[Any]]

  /**
   * @return an array of the continuous variables of this clique.
   */
  def contVars: Array[ContVar]

  /**
   * @return an array of the vector variables of this clique.
   */
  def vectVars: Array[VectVar]

  /**
   * Convenience method to create setting objects corresponding to the signature of the potential.
   * @return a setting that has as many discrete, continuous and vector settings as the potential has corresponding
   *         variables.
   */
  def createSetting() = new Setting(discVars.length, contVars.length, vectVars.length)

  def createPartialSetting() = new PartialSetting(discVars.length, contVars.length, vectVars.length)

  /**
   * Convert a setting to state.
   * @param setting the setting to convert.
   * @return a state that maps variables to values according to the
   */
  def toState(setting: Setting) = {
    val disc = for ((v, i) <- discVars.iterator.zipWithIndex) yield v -> v.dom(setting.disc(i))
    val cont = for ((v, i) <- contVars.iterator.zipWithIndex) yield v -> setting.cont(i)
    val vect = for ((v, i) <- vectVars.iterator.zipWithIndex) yield v -> setting.vect(i)
    State((disc ++ cont ++ vect).toMap)
  }

  /**
   * Convert a state into a partial setting.
   * @param state the state to convert.
   * @return a partial setting in which only those slots are observed which correspond to variables
   *         that have an assignment in the state.
   */
  def toPartialSetting(state: State) = {
    val result = createPartialSetting()
    for ((v, i) <- discVars.iterator.zipWithIndex) state.get(v).foreach(value => {
      result.discObs(i) = true
      result.disc(i) = v.dom.indexOf(value)
    })
    for ((v, i) <- contVars.iterator.zipWithIndex) state.get(v).foreach(value => {
      result.contObs(i) = true
      result.cont(i) = value
    })
    for ((v, i) <- vectVars.iterator.zipWithIndex) state.get(v).foreach(value => {
      result.vectObs(i) = true
      result.vect(i) = value
    })
    result
  }

  /**
   * Create a new set of empty messages.
   * @return empty messages for this clique.
   */
  def createMsgs() = new Msgs(
    discVars.map(v => new DiscMsg(v.dom.size)).toArray,
    discVars.map(v => new ContMsg()).toArray,
    discVars.map(v => new VectMsg()).toArray)

}

/**
 * A Scorer is in charge of calculating the scores that a potential assigns to each state.
 */
trait Scorer {
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
 */
trait StatelessScorer extends Potential with Scorer {
  def scorer() = this
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
trait ExpFamScorer extends Statistics with Scorer {

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
trait ExpFamPotential extends Potential {
  def scorer(): ExpFamScorer
}


/**
 * Convenience trait for potentials that only connect discrete variables.
 */
trait DiscPotential extends Potential {
  val contVars = Potential.emptyContVars
  val vectVars = Potential.emptyVectVars
}

/**
 * Convenience trait for potentials that only connect continuous variables.
 */
trait ContPotential extends Potential {
  val discVars = Potential.emptyDiscVars
  val vectVars = Potential.emptyVectVars
}

/**
 * Convenience trait for potentials that only connect vector variables.
 */
trait VectPotential extends Potential {
  val discVars = Potential.emptyDiscVars
  val contVars = Potential.emptyContVars
}


/**
 * Companion object for potentials
 */
object Potential {
  lazy val emptyDiscVars = Array.ofDim[DiscVar[Any]](0)
  lazy val emptyContVars = Array.ofDim[ContVar](0)
  lazy val emptyVectVars = Array.ofDim[VectVar](0)
}

/**
 * An argmaxer can find the argmax of some function given observations and incoming messages.
 */
trait Argmaxer {
  def argmax(observed: PartialSetting, incoming: Msgs, result: Setting, score: DoubleBuffer)
}


object Argmax {

  /**
   * Convenience method for argmax calculation that works with state objects instead of settings, and which always creates fresh
   * objects.
   * @param space the search space that defines the variables to search over, and is responsible for mapping states to values.
   * @param pot the potential to argmax.
   * @param observation the observed state.
   * @return the result of an argmax.
   */
  def apply[T](space: SearchSpace[T], observation: State = State.empty)(pot: SupportsArgmax): T = {
    val argmaxer = pot.argmaxer
    val result = pot.createSetting()
    val scoreBuffer = new DoubleBuffer()
    argmaxer.argmax(pot.toPartialSetting(observation), pot.createMsgs(), result, scoreBuffer)
    val state = pot.toState(result)
    space.toValue(state)
  }
}

/**
 * A sum of potential functions forms a potential as well.
 * @tparam P the type of argument potentials.
 */
trait Sum[P <: Potential] extends Potential {
  def args: Seq[P]
  lazy val discVars = args.flatMap(_.discVars).distinct.toArray
  //distinct does not work with iterators
  lazy val contVars = args.flatMap(_.contVars).distinct.toArray
  lazy val vectVars = args.flatMap(_.vectVars).distinct.toArray

  lazy val discVar2Index = discVars.iterator.zipWithIndex.toMap
  lazy val contVar2Index = contVars.iterator.zipWithIndex.toMap
  lazy val vectVar2Index = vectVars.iterator.zipWithIndex.toMap


  lazy val argMaps = args.map(a => new ArgMap(
    a.discVars.map(discVar2Index),
    a.contVars.map(contVar2Index),
    a.vectVars.map(vectVar2Index)))

  def scorer() = new Scorer {

    lazy val scorers = args.map(_.scorer())

    def score(setting: Setting) = {
      val scores = for (((arg, map), scorer) <- (args.iterator zip argMaps.iterator) zip scorers.iterator) yield {
        val local = arg.createSetting()
        for (i <- 0 until arg.discVars.length) local.disc(i) = setting.disc(map.discArgs(i))
        for (i <- 0 until arg.contVars.length) local.cont(i) = setting.cont(map.contArgs(i))
        for (i <- 0 until arg.vectVars.length) local.vect(i) = setting.vect(map.vectArgs(i))
        val localScore = scorer.score(local)
        localScore
      }
      scores.sum
    }
  }

}


class FlatSum[P <: Potential](val args: Seq[P]) extends Sum[P]

trait DifferentiableFlatSum[P <: StatelessDifferentiable] extends Sum[P] with StatelessDifferentiable {
  //rather define it as GradientCalculator ?
  def gradientAndValue(currentParameters: PartialSetting, gradient: Setting): Double = {
    val totalUpdate = new Setting(discVars.length, contVars.length, vectVars.length)
    val scores = for ((arg, map) <- args zip argMaps) yield {
      //could not get it to work with iterators (?)
      val localUpdate = new Setting(arg.discVars.length, arg.contVars.length, arg.vectVars.length)
      val local = arg.createSetting()
      for (i <- 0 until arg.discVars.length) local.disc(i) = currentParameters.disc(map.discArgs(i))
      for (i <- 0 until arg.contVars.length) local.cont(i) = currentParameters.cont(map.contArgs(i))
      for (i <- 0 until arg.vectVars.length) local.vect(i) = currentParameters.vect(map.vectArgs(i))
      val localScore = arg.gradientAndValue(currentParameters, localUpdate)
      for (i <- 0 until arg.discVars.length) if (totalUpdate.disc(map.discArgs(i)) != null) totalUpdate.disc(map.discArgs(i)) += localUpdate.disc(i) else totalUpdate.disc(map.discArgs(i)) = localUpdate.disc(i) //todo be more efficient
      for (i <- 0 until arg.contVars.length) if (totalUpdate.cont(map.contArgs(i)) != null) totalUpdate.cont(map.contArgs(i)) += localUpdate.cont(i) else totalUpdate.cont(map.contArgs(i)) = localUpdate.cont(i)
      for (i <- 0 until arg.vectVars.length) if (totalUpdate.vect(map.vectArgs(i)) != null) totalUpdate.vect(map.vectArgs(i)) += localUpdate.vect(i) else totalUpdate.vect(map.vectArgs(i)) = localUpdate.vect(i)
      localScore
    }
    for (i <- 0 until this.discVars.length) gradient.disc(i) = totalUpdate.disc(i) //todo, change only the ones that change
    for (i <- 0 until this.contVars.length) gradient.cont(i) = totalUpdate.cont(i)
    for (i <- 0 until this.vectVars.length) gradient.vect(i) = totalUpdate.vect(i)
    scores.sum
  }
}


class GenericDiscretePotential(val discVars: Array[DiscVar[Any]], scoring: Setting => Double) extends DiscPotential {

  def scorer() = new Scorer {
    def score(setting: Setting) = scoring(setting)
  }
}


