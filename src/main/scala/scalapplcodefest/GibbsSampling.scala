package scalapplcodefest

import cc.factorie._
import scalapplcodefest.legacy.term._
import scalapplcodefest.TermConverter._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scalapplcodefest.legacy.term.LambdaAbstraction
import scala.collection.mutable
import scalapplcodefest.legacy.value.{Doubles, Fun}
import scala.collection.immutable.HashMap
import scalapplcodefest.TermDSL.doubles

/**
 * Perform basic gibbs sampling on a term double
 * @author sameer
 */
case class GibbsSampling[T](term: LambdaAbstraction[T, Double])(implicit random: Random) {

  import scalapplcodefest.InferenceUtils.Counts

  val varTermMap = new mutable.HashMap[Variable[Any], ArrayBuffer[Term[Double]]]
  val normalized = normalizeTerm(term.body)
  val sigVars = term.sig.variables
  val vars = sigVars.toSeq.sorted(VariableOrdering)

  def normalizeTerm(t: Term[Double]): Term[Double] = {
    val conditionsPushed = pushDownConditions(t)
    val flat = flatten(conditionsPushed, doubles.add)
    val dotsPushed = pushDownDotProducts(flat)
    val grouped = groupLambdas(dotsPushed)
    val brackets = bracketInsideLambda(grouped)
    val unrolled = unrollLambdaImages(brackets)
    val result = flatten(unrolled, doubles.add)
    val unbracketed = unbracket(result)
    val terms = asSeq(unbracketed, doubles.add)
    for(t <- terms) {
      for(v <- t.variables) {
        varTermMap.getOrElseUpdate(v, new ArrayBuffer) += t
      }
    }
    unbracketed
  }

  def score(s: State, changed: Variable[Any]): Double = {
    varTermMap(changed).map(_.value(s)).sum
    //normalized.value(s)
  }

  def sample(curr: State): State = {
    // pick a random variable
    val variable = vars.sampleUniformly
    val currValue = curr(variable)
    // iterate through the values
    val values = variable.domain.value(curr).toSeq
    val scores = Array.ofDim[Double](values.length)
    for ((v, i) <- values.zipWithIndex) {
      val newState = if (currValue == v) curr else State.single(variable, v) + curr
      scores(i) = score(newState, variable)
    }
    // sample
    val nv = scores.toSeq.zip(values).sampleExpProportionally(_._1)._2
    if (currValue == nv) curr else State.single(variable, nv) + curr
  }

  def infer(init: State, numSamples: Int, burn: Int, thinning: Int): State = {
    assert(thinning >= 1, "Thinning %d should be > 0" format (thinning))
    assert(numSamples >= 1, "Num Samples %d should be > 0" format (numSamples))
    assert(burn >= 0, "Burn-in %d should be >= 0" format (burn))
    var curr = if (init == State.empty) State(vars.map(v => (v, v.default)).toMap) else init
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
    InferenceUtils.convertCountsToState(counts)
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

}

object InferenceUtils {
  type Counts = mutable.HashMap[Variable[Any], mutable.HashMap[Any, Double]]

  def convertCountsToState(counts: Counts): State = {
    val map = for (variable <- counts.keysIterator) yield {
      val cs = counts(variable)
      val norm = cs.map(_._2).sum
      val margs = cs.map(p => (p._1 -> p._2 / norm))
      val fun = Fun(margs.toMap, variable.domain.value(), Doubles)
      Belief(variable) -> fun
    }
    State(map.toMap)
  }

  def valuesIterator(vars: Iterable[Variable[_]]): Iterator[State] = State.allStates(vars.toList).iterator
}

case class BruteForceMarginalInference[T](term: LambdaAbstraction[T, Double]) {

  import scalapplcodefest.InferenceUtils.Counts

  val normalized = pushDownConditions(term.body)
  val sigVars = term.sig.variables
  val vars = sigVars.toSeq.sorted(VariableOrdering)

  def infer: State = {
    val margs = new Counts
    var Z = 0.0
    for (s <- InferenceUtils.valuesIterator(vars)) {
      val score = normalized.value(s)
      val escore = StrictMath.exp(score)
      Z += escore
      for (v <- vars) {
        val m = margs.getOrElseUpdate(v, new mutable.HashMap)
        val value = s(v)
        m(value) = m.getOrElse(value, 0.0) + escore
      }
    }
    // normalize the marginals
    for ((v, m) <- margs) {
      for (value <- m.keysIterator) {
        m(value) = m(value) / Z
      }
    }
    InferenceUtils.convertCountsToState(margs)
  }
}