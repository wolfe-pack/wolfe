package scalapplcodefest

import cc.factorie._
import scalapplcodefest.term._
import scalapplcodefest.TermConverter._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scalapplcodefest.term.LambdaAbstraction
import scala.collection.mutable
import scalapplcodefest.value.{Doubles, Fun}
import scala.collection.immutable.HashMap

/**
 * Perform basic gibbs sampling on a term double
 * @author sameer
 */
case class GibbsSampling[T](term: LambdaAbstraction[T, Double])(implicit random: Random) {

  val normalized = pushDownConditions(term.body)
  val ForceLinear(coefficient, parameters, base) = normalized
  val sigVars = term.sig.variables
  val vars = (coefficient.variables ++ base.variables).filter(sigVars).toSeq.sorted(VariableOrdering)

  type Counts = mutable.HashMap[Variable[Any], mutable.HashMap[Any, Double]]

  def sample(curr: State): State = {
    // pick a random variable
    val variable = vars.sampleUniformly
    val currValue = curr(variable)
    // iterate through the values
    val values = variable.domain.value(curr).toSeq
    val scores = new ArrayBuffer[Double](values.length)
    for ((v, i) <- values.zipWithIndex) {
      val newState = if (currValue == v) curr else State.single(variable, v) + curr
      scores(i) = normalized.value(newState)
    }
    // sample
    val nv = scores.zip(values).sampleExpProportionally(_._1)
    if (currValue == nv) curr else State.single(variable, nv) + curr
  }

  def infer(init: State, numSamples: Int, burn: Int, thinning: Int): State = {
    assert(thinning >= 1, "Thinning %d should be > 0" format (thinning))
    assert(numSamples >= 1, "Num Samples %d should be > 0" format (numSamples))
    assert(burn >= 0, "Burn-in %d should be >= 0" format (burn))
    var curr = init
    for (i <- 0 until burn) {
      curr = sample(curr)
    }
    val counts = new Counts
    for (i <- 0 until numSamples) {
      accumulate(curr, counts)
      curr = accumulateState(curr)
      for (j <- 0 until thinning) {
        curr = sample(curr)
      }
    }
    // convert counts to state
    convertCountsToState(counts)
  }

  def accumulate(state: State, counts: Counts) {
    for (variable <- vars) {
      val value = state(variable)
      val map = counts.getOrElseUpdate(variable, new mutable.HashMap)
      map(value) = map.getOrElse(value, 0.0) + 1.0
    }
  }

  def accumulateState(state: State): State = {
    val map = HashMap.apply(vars.map(v => (v, state(v))): _*)
    State(map)
  }

  def convertCountsToState(counts: Counts): State = {
    val map = for (variable <- vars) yield {
      val cs = counts(variable)
      val norm = cs.map(_._2).sum
      val margs = cs.map(p => (p._1 -> p._2 / norm))
      val fun = Fun(margs.toMap, variable.domain.value(), Doubles)
      Belief(variable) -> fun
    }
    State(map.toMap)
  }
}
