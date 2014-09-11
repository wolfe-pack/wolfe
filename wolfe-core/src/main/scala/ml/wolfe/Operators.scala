package ml.wolfe

/**
 * A set of operators used in typical machine learning workflows. This trait provides
 * default implementations to all operators. These (a) serve as a definition of the semantics
 * of the operator, and (b) are used in [[ml.wolfe.BruteForceOperators]] for clients to access directly.
 *
 * @author Sebastian Riedel
 */
trait Operators {

  import scala.language.implicitConversions

  import Wolfe._

  /**
   * Returns the argmax of the given objective over the given domain
   * @param dom the domain to argmax over.
   * @param obj the objective to optimize
   * @tparam T the type of objects in the domain.
   * @tparam N the type of values the objective returns (usually Doubles)
   * @return an element `x` in `dom` with maximal objective value `obj(x)`.
   */
  def argmax[T, N: Ordering](dom: Iterable[T])(obj: T => N) = dom.maxBy(obj)

  /**
   * Returns the argmin of the given objective over the given domain
   * @param dom the domain to argmin over.
   * @param obj the objective to optimize
   * @tparam T the type of objects in the domain.
   * @tparam N the type of values the objective returns (usually Doubles)
   * @return an element `x` in `dom` with minimal objective value `obj(x)`.
   */
  def argmin[T, N: Ordering](dom: Iterable[T])(obj: T => N) = dom.minBy(obj)

  /**
   * Applies a function to a sequence of values and sums up the results.
   * @param dom the values that the `obj` function will be applied to.
   * @param obj the function to apply to the values.
   * @tparam T type of objects in the sequence of values.
   * @tparam N return type of `obj`, usually `Double` or `Vector`.
   * @return sum of `obj(x)` over all `x` in `dom`.
   */
  def sum[T, N: Numeric](dom: Iterable[T])(obj: T => N) = dom.foldLeft(implicitly[Numeric[N]].zero)( (acc, t) =>
    implicitly[Numeric[N]].plus(acc, obj(t)))

  /**
   * Calculates the log-partition function of a log-linear model.
   * @param dom the domain of objects to sum over.
   * @param obj the unnormalized log-linear model to calculate the log partition function for.
   * @tparam T type of objects in domain.
   * @return the logarithm of the partition function of `obj` with respect to domain `dom`.
   */
  def logZ[T](dom: Iterable[T])(obj: T => Double) = math.log((dom map (x => math.exp(obj(x)))).sum)

  /**
   * Returns the maximal value of a function with respect to a domain.
   * @param dom the domain to max over.
   * @param obj the objective to maximize.
   * @tparam T the type of objects in the domain.
   * @tparam N the type of values the objective returns (usually Double).
   * @return value in `dom` that maximizes `obj`.
   */
  def max[T, N: Ordering](dom: Iterable[T])(obj: T => N) = dom.map(obj).max

  /**
   * Applies a mapping function to an iterable. This method is useful when the mapping function contains
   * macro code that may be reused for different elements in the collection. For example, the mapping function
   * may need a weight vector that can be created once and then used for each element.
   *
   * @see [[ml.wolfe.macros.OptimizedOperators]]
   * @param dom the iterable of objects to be mapped.
   * @param mapper the mapping function.
   * @tparam A the type of objects in the original iterable.
   * @tparam B the return type of the mapping function.
   * @return an iterable that contains the results of applying the mapping function to the domain.
   */
  def map[A, B](dom: Iterable[A])(mapper: A => B): Iterable[B] = dom.map(mapper)

  /**
   * Calculates expectations of statistics under a log-linear model.
   * @param dom the domain to calculate the expectations over.
   * @param logLinear the linear scoring function of the log-linear model. This is the unnormalized probability model in log-space.
   * @param stats a function that returns statistics of a sample. This can contain statistics that check whether
   *              certain variables are in certain states, and hence we can use this method to get marginal probabilities.
   * @tparam T type of objects in domain.
   * @return the expectation of the given statistics under the given model.
   */
  def expect[T](dom: Iterable[T])(logLinear: T => Double)(stats: T => Vector):Vector = {
    val lZ = logZ(dom)(logLinear)
    sum(dom) { x => stats(x) * math.exp(logLinear(x) - lZ) }
  }


}

/**
 * This is a concrete implementation of the operators that uses the default methods of [[ml.wolfe.Operators]].
 */
object BruteForceOperators extends Operators