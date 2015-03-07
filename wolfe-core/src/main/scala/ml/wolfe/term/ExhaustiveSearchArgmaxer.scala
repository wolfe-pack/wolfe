package ml.wolfe.term

import ml.wolfe.term


/**
 * @author riedel
 */
class ExhaustiveSearchArgmaxer(val obj: DoubleTerm, val wrt: Seq[Var[Dom]]) extends Argmaxer {

  import ml.wolfe.term.ExhaustiveSearch.AllSettings

  val observedVars = obj.vars.filterNot(wrt.contains)
  val settingsToVary = obj.vars.map(_.domain.createSetting()).toArray
  val obs2vars = VariableMapping(observedVars, obj.vars)
  val vars2result = VariableMapping(obj.vars, wrt)
  val newDiscAtoms = wrt.flatMap(_.atoms.disc).map(a => ExhaustiveSearch.IndexedAtom(a, obj.vars.indexOf(a.ownerOrSelf)))
  val allSettings = new AllSettings(newDiscAtoms.toIndexedSeq)
  val evaluator = obj.evaluator()
  val score = obj.domain.createSetting()

  def argmax(observed: Array[Setting], msgs: Array[Msg], result: Array[Setting]) = {
    //todo: take into account messages
    //copy observed into settingsToVary
    obs2vars.copyForwardDeep(observed, settingsToVary)
    var max = Double.NegativeInfinity
    //get discrete atoms to vary
    allSettings.iterate(settingsToVary) {
      evaluator.eval(settingsToVary, score)
      if (score.cont(0) > max) {
        max = score.cont(0)
        vars2result.copyForwardDeep(settingsToVary, result)
      }
    }
  }

}


object ExhaustiveSearch {

  case class IndexedAtom(atom: DiscVar[Any], variableIndex: Int)

  class AllSettings(atoms: IndexedSeq[IndexedAtom]) {
    //val flattened = for (i <- 0 until atoms.length; j <- 0 until atoms(i).length) yield (atoms(i)(j), i)
    val length = atoms.length

    def iterate(target: Array[Setting])(body: => Unit): Unit = {
      //flatten
      var index = length - 1

      var atomVarIndex = atoms(index)
      var varIndex = atomVarIndex.variableIndex
      var atom = atomVarIndex.atom

      def updateAtom(): Unit = {
        atomVarIndex = atoms(index)
        varIndex = atomVarIndex.variableIndex
        atom = atomVarIndex.atom
      }
      def currentTarget = target(varIndex).disc(atom.offset)
      def setTarget(value: Int) = target(varIndex).disc(atom.offset) = value
      def incrementTarget() = target(varIndex).disc(atom.offset) += 1
      def currentDim = atom.domain.domainSize

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

  class AllSettingsIterable[T](val atoms: IndexedSeq[IndexedAtom], val toVary: Array[Setting], fun: Array[Setting] => T) extends Iterable[T] {
    //val flattened = for (i <- 0 until atoms.length; j <- 0 until atoms(i).length) yield (atoms(i)(j), i)
    val atomsLength = atoms.length

    def iterator = new Iterator[T] {
      var index = atomsLength - 1
      var atomVarIndex = atoms(index)
      var varIndex = atomVarIndex.variableIndex
      var atom = atomVarIndex.atom

      def updateAtom(): Unit = {
        atomVarIndex = atoms(index)
        varIndex = atomVarIndex.variableIndex
        atom = atomVarIndex.atom
      }

      def currentTarget = toVary(varIndex).disc(atom.offset)

      def setTarget(value: Int) = toVary(varIndex).disc(atom.offset) = value

      def incrementTarget() = toVary(varIndex).disc(atom.offset) += 1

      def currentDim = atom.domain.domainSize

      def hasNext = index >= 0

      def next() = {
        val result = fun(toVary)
        while (index >= 0 && (currentTarget == currentDim - 1)) {
          setTarget(0)
          index -= 1
          if (index >= 0) updateAtom()
        }
        //increase setting by one if we haven't yet terminated
        if (index >= 0) {
          incrementTarget()
          //depending on where we are in the array we bump up the settingId
          if (index < atomsLength - 1) {
            index = atomsLength - 1
            updateAtom()
          }
        }
        result
      }
    }

  }


}

/**
 * @author riedel
 */
class ExhaustiveSearchMaxMarginalizer(val obj: DoubleTerm, val wrt: Seq[Var[Dom]], target: Seq[Var[Dom]]) extends MaxMarginalizer {

  import ml.wolfe.term.ExhaustiveSearch.AllSettings

  require(wrt.forall(_.domain.isDiscrete), "Cannot do exhaustive search over continuous domains")
  val observedVars = obj.vars.filterNot(v => wrt.contains(v) || target.contains(v))
  val settingsToVary = obj.vars.map(_.domain.createSetting()).toArray
  val obs2full = VariableMapping(observedVars, obj.vars)
  val wrt2full = VariableMapping(wrt, obj.vars)
  val tgt2full = VariableMapping(target, obj.vars)
  val varyingVars = (wrt ++ target).distinct
  val newDiscAtoms = varyingVars.flatMap(_.atoms.disc).map(a => ExhaustiveSearch.IndexedAtom(a, obj.vars.indexOf(a.ownerOrSelf)))
  val allSettings = new AllSettings(newDiscAtoms.toIndexedSeq)
  val evaluator = obj.evaluator()
  val score = obj.domain.createSetting()


  def maxMarginals(observed: Array[Setting], wrtMsgs: Array[Msg], targetMsgs: Array[Msg]) = {
    for (i <- 0 until targetMsgs.length) targetMsgs(i) := Double.NegativeInfinity
    obs2full.copyForwardDeep(observed, settingsToVary)
    allSettings.iterate(settingsToVary) {
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
          tgt.msg(currentSetting) = math.max(tgt.msg(currentSetting), penalized)
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

/**
 * @author riedel
 */
class ExhaustiveSearchMaxMarginalizer2(val obj: DoubleTerm, val wrt: Seq[Var[Dom]],val observed: Seq[Var[Dom]],
                                       val input: Settings, val inputMsgs: Msgs) extends MaxMarginalizer2 {

  require(wrt.forall(_.domain.isDiscrete), "Cannot do exhaustive search over continuous domains")
  val target = obj.vars.filterNot(v => wrt.contains(v) || observed.contains(v))
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

  def maxMarginals()(implicit execution: Execution) = {
    for (i <- 0 until outputMsgs.length) outputMsgs(i) := Double.NegativeInfinity

    allSettings.loopSettings { settings =>
      objEval.eval()
      //add penalties from incoming messages based on current setting
      var penalized = objEval.output.cont(0)
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
          tgt.msg(currentValue) = math.max(tgt.msg(currentValue), penalized)
        }
      }
    }
  }


}
