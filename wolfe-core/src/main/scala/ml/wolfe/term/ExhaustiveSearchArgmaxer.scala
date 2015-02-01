package ml.wolfe.term

import ml.wolfe.fg20.{Msgs, Setting}

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
    allSettings(discAtoms)(settingsToVary) {
      evaluator.eval(settingsToVary, score)
      if (score.cont(0) > max) {
        max = score.cont(0)
        vars2result.copyForward(settingsToVary,result)
      }
    }
  }

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

  def allSettings(dims: Array[Int], observations: Array[Boolean])(target: Array[Int])(body: Int => Unit): Unit = {
    val length = target.length
    var settingId = 0
    var settingMultiplier = 1
    var index = length - 1
    //set observations
    while (index >= 0) {
      if (observations(index)) {
        settingId += settingMultiplier * target(index)
      } else target(index) = 0
      settingMultiplier *= dims(index)
      index -= 1
    }
    settingMultiplier = 1
    index = length - 1
    while (index >= 0) {
      //call body on current setting
      body(settingId)
      //go back to the first element that hasn't yet reached its dimension, and reset everything until then
      while (index >= 0 && (target(index) == dims(index) - 1 || observations(index))) {
        if (!observations(index)) {
          settingId -= settingMultiplier * target(index)
          target(index) = 0
        }
        settingMultiplier *= dims(index)
        index -= 1
      }
      //increase setting by one if we haven't yet terminated
      if (index >= 0) {
        target(index) += 1
        settingId += settingMultiplier
        //depending on where we are in the array we bump up the settingId
        if (index < length - 1) {
          settingMultiplier = 1
          index = length - 1
        }
      }
    }
  }


}
