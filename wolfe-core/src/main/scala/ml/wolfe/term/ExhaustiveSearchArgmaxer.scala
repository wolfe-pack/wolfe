package ml.wolfe.term

/**
 * @author riedel
 */
class ExhaustiveSearchArgmaxer(val obj: DoubleTerm, val wrt: Seq[Var[Dom]]) extends Argmaxer {
  val observedVars   = obj.vars.filterNot(wrt.contains)
  val settingsToVary = obj.vars.map(_.domain.createSetting()).toArray
  val obs2vars       = VariableMapping(observedVars, obj.vars)
  val vars2result    = VariableMapping(obj.vars, wrt)
  val discAtoms      = wrt.map(_.atoms.disc.toArray).toArray
  val evaluator      = obj.evaluator()
  val score          = obj.domain.createSetting()
  def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting]) = {
    //todo: take into account messages
    //copy observed into settingsToVary
    obs2vars.copyForward(observed, settingsToVary)
    var max = Double.NegativeInfinity
    //get discrete atoms to vary
    ExhaustiveSearch.allSettings(discAtoms)(settingsToVary) {
      evaluator.eval(settingsToVary, score)
      if (score.cont(0) > max) {
        max = score.cont(0)
        vars2result.copyForward(settingsToVary, result)
      }
    }
  }

}

object ExhaustiveSearch {
  def allSettings(atoms: Array[Array[DiscVar[Any]]])(target: Array[Setting])(body: => Unit): Unit = {
    //flatten
    val flattened = for (i <- 0 until atoms.length; j <- 0 until atoms(i).length) yield (atoms(i)(j), i)
    val length = flattened.length
    var index = length - 1

    var atomVarIndex = flattened(index)
    var varIndex = atomVarIndex._2
    var atom = atomVarIndex._1

    def updateAtom(): Unit = {
      atomVarIndex = flattened(index)
      varIndex = atomVarIndex._2
      atom = atomVarIndex._1
    }
    def currentTarget = target(varIndex).disc(atom.offset)
    def setTarget(value: Int) = target(varIndex).disc(atom.offset) = value
    def incrementTarget() = target(varIndex).disc(atom.offset) += 1
    def currentDim = atom.domain.values.length

    while (index >= 0) {
      //call body on current setting
      body
      //go back to the first element that hasn't yet reached its dimension, and reset everything until then
      while (index >= 0 && (currentTarget == currentDim - 1)) {
        setTarget(0)
        index -= 1
        if (index >= 0) updateAtom()
      }
      //increase setting by one if we haven't yet terminated
      if (index >= 0) {
        incrementTarget()
        //depending on where we are in the array we bump up the settingId
        if (index < length - 1) {
          index = length - 1
          updateAtom()
        }
      }
    }
  }

}

/**
 * @author riedel
 */
class ExhaustiveSearchMaxMarginalizer(val obj: DoubleTerm, val wrt: Seq[Var[Dom]], target: Seq[Var[Dom]]) extends MaxMarginalizer {
  require(obj.domain.isDiscrete, "Cannot do exhaustive search over continuous domains")
  val observedVars   = obj.vars.filterNot(v => wrt.contains(v) || target.contains(v))
  val settingsToVary = obj.vars.map(_.domain.createSetting()).toArray
  val obs2full       = VariableMapping(observedVars, obj.vars)
  val wrt2full       = VariableMapping(wrt, obj.vars)
  val tgt2full       = VariableMapping(target, obj.vars)
  val discAtoms      = wrt.map(_.atoms.disc.toArray).toArray
  val evaluator      = obj.evaluator()
  val score          = obj.domain.createSetting()


  def maxMarginals(observed: Array[Setting], wrtMsgs: Array[Msgs], targetMsgs: Array[Msgs]) = {
    for (i <- 0 until targetMsgs.length) targetMsgs(i) := -Double.NegativeInfinity
    obs2full.copyForward(observed, settingsToVary)
    ExhaustiveSearch.allSettings(discAtoms)(settingsToVary) {
      evaluator.eval(settingsToVary, score)
      var penalized = score.cont(0)

      //add penalties from incoming messages based on current setting
      for (wrtIndex <- 0 until wrtMsgs.length) {
        val indexInFullSetting = wrt2full.getTgtIndex(wrtIndex)
        for (i <- 0 until wrtMsgs(wrtIndex).disc.length) {
          val currentSetting = settingsToVary(indexInFullSetting).disc(i)
          val currentMsg = wrtMsgs(wrtIndex).disc(i).msg(currentSetting)
          penalized += currentMsg
        }
      }

      //now update outgoing messages with the max of their current value and the new score
      for (tgtIndex <- 0 until targetMsgs.length) {
        val indexInFullSetting = tgt2full.getTgtIndex(tgtIndex)
        for (i <- 0 until targetMsgs.length) {
          val currentSetting = settingsToVary(indexInFullSetting).disc(i)
          val tgt = targetMsgs(tgtIndex).disc(i)
          tgt.msg(currentSetting) = math.max(tgt.msg(currentSetting),penalized)
          val currentMsg = tgt.msg(currentSetting)
        }
      }
    }
  }

  /*
        fill(result.msg, Double.NegativeInfinity)
      TablePotential.allSettings(dims, partialSetting.discObs)(partialSetting.disc) { i =>
        var score = scoreTableEntry(i, partialSetting)
        val varValue = partialSetting.disc(varIndex)
        for (j <- 0 until partialSetting.disc.length; if j != varIndex) {
          score += incoming.disc(j).msg(partialSetting.disc(j))
        }
        result.msg(varValue) = math.max(score, result.msg(varValue))
      }
      maxNormalize(result.msg)
     */


}
