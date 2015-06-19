package ml.wolfe.term

import com.typesafe.scalalogging.slf4j.LazyLogging
import ml.wolfe.util.ProgressBar

import scala.math._
import scala.util.Random

case class AdaGradParameters(epochs: Int,
                             learningRate: Double,
                             delta: Double = 0.0,
                             initParams: Settings = new Settings(0),
                             epochHook: (IndexedSeq[Any], Int) => String = null,
                             adaptiveVectors: Boolean = true,
                             optimizeTerm: Boolean = false)

/**
 * @author riedel
 */
class AdaGradArgmaxer(val objRaw: DoubleTerm,
                      val wrt: Seq[Var[Dom]],
                      val observed: Settings,
                      val msgs: Msgs)(implicit params: AdaGradParameters) extends Argmaxer with LazyLogging {

  import params._
  import Transformer._

  private val random = new Random(0)

  def stochastifySum(term: DoubleTerm) = {
    term match {
      case FirstOrderSum(range, variable, body) =>
        val sampled = VarSeqApply[Dom,Term[VarSeqDom[Dom]],IntTerm](range, range.domain.indexDom.shuffled(random))//range.sampleShuffled(random)
        replace(body)(variable,sampled)
      case _ => term
    }
  }


  val preprocess = optimizeTerm match {
    case true =>
      //I think this hasn't been tested for a while, and the order of clean/precalculate seems different
      stochastifySum _ andThen clean andThen precalculate andThen groundSums andThen flattenSums
    case false =>
      stochastifySum _ andThen precalculate andThen clean andThen flattenSums // //andThen Transformer.clean _ andThen
  }
  val obj = preprocess(objRaw)

  val obsVars = obj.vars.filterNot(wrt.contains)
  //get differentiator

  val initParameters = if (initParams.isEmpty) Settings.fromSeq(wrt.map(_.domain.createZeroSetting())) else initParams

  val obs2full = VariableMapping(obsVars, obj.vars)
  val param2full = VariableMapping(wrt, obj.vars)

  val scale = new Setting(numCont = 1)

  val gradient = Settings.fromSeq(wrt.map(_.domain.createSparseZeroSetting()))
  val momentum = Settings.fromSeq(wrt.map(_.domain.createZeroSetting()))

  val termsPerEpoch = Traversal.distinctSampleCount(obj)

  logger.info("Terms per epoch: " + termsPerEpoch)

  val iterations = epochs * termsPerEpoch
  var objAccumulator = 0.0

  val result = Settings.fromSeq(wrt.map(_.domain.createSetting()))

  obs2full.linkTargetsToSource(observed, result)

  if (adaptiveVectors) gradient.foreach(_.setAdaptiveVectors(true))

  param2full.copyForwardDeep(initParameters, result)

  val diff = obj.differentiatorImpl(wrt)(result, scale, gradient)

  val gradientChangeRecorders = gradient map (s => new SettingChangeRecorder(s, false))

  def argmax()(implicit execution: Execution) = {
    logger.info("Maximizing objective")
    val bar = new ProgressBar(epochs, if (epochs < 100) 1 else epochs / 100)
    bar.start()

    scale.cont(0) = 1.0

    //now optimize
    for (iteration <- 0 until iterations) {
      val epoch = iteration / termsPerEpoch

      //reset all previous changes to the gradient back to zero, and then forget changes
      gradientChangeRecorders foreach (_.setChangesToZero())
      gradientChangeRecorders foreach (_.forget())

      //add term gradient into result gradient
      diff.differentiate()(Execution(iteration, Execution.Diff))
      assert(!diff.output.cont(0).isNaN)

      //now update the momentum, need to call atoms again because atoms may have changed if objective is dynamic
      addSquared(gradient, momentum)

      //now add gradient into result parameters, using momentum to determine learning rate.
      gradientStep(epoch, gradient, momentum, result, learningRate)

      objAccumulator += diff.output.cont(0)
      assert(!objAccumulator.isNaN)

      //logger.info(s"${iteration % termsPerEpoch}/$termsPerEpoch\r")

      if ((iteration + 1) % termsPerEpoch == 0) {
        if (epochHook != null) {
          val parameters = for ((v, s) <- wrt zip result) yield v.domain.toValue(s)
          val text = epochHook(parameters.toIndexedSeq, (iteration + 1) / termsPerEpoch)
          assert(!objAccumulator.isNaN)
          bar(s"Obj: $objAccumulator $text", lineBreak = true)
        } else {
          bar(s"Obj: $objAccumulator", lineBreak = true)
        }
        objAccumulator = 0.0
      }
    }
  }


  def addSquared(gradient: Settings, result: Settings): Unit = {
    for (i <- 0 until gradient.length) {
      for (offset <- gradientChangeRecorders(i).cont.changes) {
        val current = gradient(i).cont(offset)
        result(i).cont(offset) += current * current
      }
      for (offset <- gradientChangeRecorders(i).vect.changes) {
        val current = gradient(i).vect(offset)
        val targetVector = result(i).vect(offset)
        for (j <- current.activeDomain) {
          targetVector(j) += current(j) * current(j)
        }
      }
      for (offset <- gradientChangeRecorders(i).mats.changes) {
        val current = gradient(i).mats(offset)
        val targetMatrix = result(i).mats(offset)
        //todo: probably slow
        for (i1 <- 0 until current.dim1; i2 <- 0 until current.dim2)
          targetMatrix(i1, i2) += current(i1, i2) * current(i1, i2)
      }
    }
  }

  def gradientStep(epoch: Int, gradient: Settings, momentum: Settings, result: Settings, lambda: Double): Unit = {
    for (i <- 0 until gradient.length) {
      for (offset <- gradientChangeRecorders(i).cont.changes) {
        val g = gradient(i).cont(offset)
        val h = momentum(i).cont(offset)
        result(i).cont(offset) += lambda / (sqrt(h) + delta) * g //this should be equivalent to update and addition, so changes should be recorded
      }
      for (offset <- gradientChangeRecorders(i).vect.changes) {
        val g = gradient(i).vect(offset)
        val h = momentum(i).vect(offset)
        for (j <- g.activeDomain) {
          val grad = g(j)
          if (grad != 0.0) result(i).vect(offset)(j) += lambda / (sqrt(h(j)) + delta) * grad
        }
        result(i).vect.broadcastChange(offset)
      }
      for (offset <- gradientChangeRecorders(i).mats.changes) {
        val g = gradient(i).mats(offset)
        val h = momentum(i).mats(offset)
        //todo: probably slow
        for (i1 <- 0 until g.dim1; i2 <- 0 until g.dim2)
          result(i).mats(offset)(i1, i2) += lambda / (sqrt(h(i1, i2)) + delta) * g(i1, i2)
        result(i).mats.broadcastChange(offset)
      }
    }
  }


}
