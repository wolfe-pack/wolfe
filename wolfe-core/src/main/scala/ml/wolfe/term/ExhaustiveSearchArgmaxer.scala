package ml.wolfe.term

import ml.wolfe.term
import math._


/**
 * @author riedel
 */
abstract class ExhaustiveMessageCalculator(val obj: DoubleTerm, val wrt: Seq[Var[Dom]], val observed: Seq[Var[Dom]],
                                  val input: Settings, val inputMsgs: Msgs, reverseMsgsAlso: Boolean = false)
  extends MessageCalculator {

  def aggregate(current: Double, score: Double): Double
  def initialValue:Double

  require(wrt.forall(_.domain.isDiscrete), "Cannot do exhaustive search over continuous domains")
  val target = obj.vars.filterNot(v => (if (reverseMsgsAlso) false else wrt.contains(v)) || observed.contains(v))
  val varyingVars = (wrt ++ target).distinct

  val settingsToVary = Settings.fromSeq(varyingVars.map(_.domain.createSetting()))
  val objInput = obj.createInputSettings()

  val toVary2wrt = VariableMapping(varyingVars, wrt)
  val toVary2target = VariableMapping(varyingVars, target)
  val toVary2obj = VariableMapping(varyingVars, obj.vars)
  val obs2full = VariableMapping(observed, obj.vars)

  val allSettings = new term.AllSettings(varyingVars.map(_.domain).toIndexedSeq, settingsToVary)(_ => {})

  //link varying settings and observed settings to the input settings of the body evaluator
  toVary2obj.linkTargetsToSource(settingsToVary, objInput)
  obs2full.linkTargetsToSource(input, objInput)

  val objEval = obj.evaluatorImpl(objInput)
  val outputMsgs = Msgs(target.map(_.domain.createZeroMsg()))

  def calculate()(implicit execution: Execution) = {
    for (i <- 0 until outputMsgs.length) outputMsgs(i) := initialValue

    allSettings.loopSettings { settings =>
      objEval.eval()
      //println(settings)
      //add penalties from incoming messages based on current setting
      var penalized = objEval.output.cont(0)
      //println(penalized)
      for ((toVaryIndex, wrtIndex) <- toVary2wrt.pairs) {
        val currentSetting = settings(toVaryIndex)
        for (i <- 0 until inputMsgs(wrtIndex).disc.length) {
          val currentValue = currentSetting.disc(i)
          val currentMsg = inputMsgs(wrtIndex).disc(i).msg(currentValue)
          penalized += currentMsg
        }
      }
      //now update outgoing messages with the max of their current value and the new score
      for ((toVaryIndex, targetIndex) <- toVary2target.pairs) {
        val currentSetting = settings(toVaryIndex)
        for (i <- 0 until outputMsgs(targetIndex).disc.length) {
          val currentValue = currentSetting.disc(i)
          val tgt = outputMsgs(targetIndex).disc(i)
          var score = penalized
          if (reverseMsgsAlso) {
            // TODO: next call is quite slow
            val wrtIndex = toVary2wrt.getTgtIndex(targetIndex)
            for (i <- 0 until inputMsgs(wrtIndex).disc.length) {
              val currentValue = currentSetting.disc(i)
              val currentMsg = inputMsgs(wrtIndex).disc(i).msg(currentValue)
              score -= currentMsg
            }
          }
          tgt.msg(currentValue) = aggregate(tgt.msg(currentValue), score) //math.max(tgt.msg(currentValue), score)
        }
      }
    }
  }

}

class ExhaustiveSearchMaxMarginalizer(obj: DoubleTerm, wrt: Seq[Var[Dom]], observed: Seq[Var[Dom]],
                                input: Settings, inputMsgs: Msgs, reverseMsgsAlso: Boolean = false)
  extends ExhaustiveMessageCalculator(obj,wrt,observed,input,inputMsgs,reverseMsgsAlso) with MaxMarginalizer {
  def aggregate(current: Double, score: Double) = max(current, score)

  def maxMarginals()(implicit execution: Execution) = calculate()

  def initialValue = Double.NegativeInfinity
}

class ExhaustiveSearchMarginalizer(obj: DoubleTerm, wrt: Seq[Var[Dom]], observed: Seq[Var[Dom]],
                             input: Settings, inputMsgs: Msgs, reverseMsgsAlso: Boolean = false)
  extends ExhaustiveMessageCalculator(obj,wrt,observed,input,inputMsgs,reverseMsgsAlso) with Marginalizer {
  def aggregate(current: Double, score: Double) = log(exp(current) + exp(score))
  def marginals()(implicit execution: Execution = Execution(0)) = calculate()
  def initialValue = Double.NegativeInfinity


}


class ExhaustiveSearchArgmaxer(val obj: DoubleTerm, val wrt: Seq[AnyVar])(val observed: Settings, val msgs: Msgs) extends Argmaxer {

  val obsVars = obj.vars.filterNot(wrt.contains)

  val wrt2obj = VariableMapping(wrt, obj.vars)
  val obs2obj = VariableMapping(obsVars, obj.vars)

  val result = Settings.fromSeq(wrt map (_.domain.createSetting()))
  val toVary = Settings.fromSeq(wrt map (_.domain.createSetting()))

  val objInput = obj.createInputSettings()

  wrt2obj.linkTargetsToSource(toVary, objInput)
  obs2obj.linkTargetsToSource(observed, objInput)

  val objEval = obj.evaluatorImpl(objInput)

  val allSettings = new term.AllSettings(wrt.map(_.domain).toIndexedSeq, toVary)(_ => {})

  def argmax()(implicit execution: Execution) = {
    var max = Double.NegativeInfinity
    allSettings.loopSettings { settings =>

      objEval.eval()
      val value = objEval.output.cont(0)

      if (value > max) {
        max = value
        result := settings
      }
    }
    //println("Argmax: " + result(0).disc.mkString(" "))

  }
}


