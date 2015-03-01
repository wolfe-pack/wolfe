package ml.wolfe.term

import ml.wolfe.util.ProgressBar

import scala.math._

case class AdaGradParameters(wrt: Seq[Var[Dom]],
                             iterations: Int,
                             learningRate: Double,
                             delta: Double,
                             initParams: Array[Setting],
                             epochHook: (IndexedSeq[Any], Int) => String = null,
                             adaptiveVectors: Boolean = true,
                             delays: Map[Atom[Dom], Int] = Map.empty)

/**
 * @author riedel
 */
class AdaGradArgmaxer2(val obj: DoubleTerm,
                       val wrt: Seq[Var[Dom]])(implicit params: AdaGradParameters)  {

  import params._

  val obsVars = obj.vars.filterNot(wrt.contains)
  //get differentiator
  val diff = obj.differentiator(wrt)

  val initParameters = if (initParams.isEmpty) wrt.map(_.domain.createZeroSetting()).toArray else initParams

  val obs2full = VariableMapping(obsVars, obj.vars)
  val param2full = VariableMapping(wrt, obj.vars)

  val scale = new Setting(numCont = 1)
  val currentValue = new Setting(numCont = 1)

  val gradient = Settings.fromSeq(wrt.map(_.domain.createZeroSetting()))
  val momentum = Settings.fromSeq(wrt.map(_.domain.createZeroSetting()))

  val var2Index = wrt.map(v => v -> obj.vars.indexOf(v)).toMap

  val termsPerEpoch = obj match {
    case _ => 1
  }

  val epochs = iterations / termsPerEpoch
  var objAccumulator = 0.0

  if (adaptiveVectors) gradient.foreach(_.setAdaptiveVectors(true))
  gradient foreach (_.recordChangedOffsets = true)

  def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Settings) = {
    val bar = new ProgressBar(epochs, if (epochs < 100) 1 else epochs / 100)
    bar.start()

    //initialize learning rate (affects gradient by changing the final upstream error signal)
    scale.cont(0) = 1.0
    //initialize with observation
//    obs2full.copyForwardDeep(observed, result)
    //initialize parameters
//    param2full.copyForwardDeep(initParameters, result)

    //now optimize
    for (iteration <- 0 until iterations) {
      val epoch = iteration / termsPerEpoch
      //reset all previous changes to the gradient
      gradient foreach (_.resetToZero())

      //add term gradient into result gradient
      //diff.addGradientAndValue(result, scale, gradient, currentValue)

      //now update the momentum, need to call atoms again because atoms may have changed if objective is dynamic
      val currentAtoms = Atoms.fromIterator(obj.atomsIterator)
      addSquaredAtoms(currentAtoms, gradient, momentum, var2Index)

      //now add gradient into result parameters, using momentum to determine learning rate.
      gradientStep(epoch, currentAtoms, gradient, momentum, result, learningRate, var2Index)

      objAccumulator += currentValue.cont(0)
      if ((iteration + 1) % termsPerEpoch == 0) {
        if (epochHook != null) {
          val parameters = for ((v, s) <- wrt zip result) yield v.domain.toValue(s)
          val text = epochHook(parameters.toIndexedSeq, (iteration + 1) / termsPerEpoch)
          bar(s"Obj: $objAccumulator $text", lineBreak = true)
        } else {
          bar(s"Obj: $objAccumulator", lineBreak = true)
        }
        objAccumulator = 0.0
      }

    }
  }

  def setAtoms(lastAtoms: Atoms, result: Array[Setting], var2Index: Map[Var[Dom], Int] = this.var2Index, value: Double = 0.0): Unit = {
    for (a <- lastAtoms.cont; i <- var2Index.get(a.ownerOrSelf)) {
      result(i).cont(a.offset) = value
    }
    for (a <- lastAtoms.vect; i <- var2Index.get(a.ownerOrSelf)) {
      if (value == 0.0)
      //        result(i).vect(a.offset) := value
        result(i).vect(a.offset).zero()
      else
        result(i).vect(a.offset) := value
    }
    for (a <- lastAtoms.mats; i <- var2Index.get(a.ownerOrSelf)) {
      result(i).mats(a.offset) := value
    }
  }

  def addSquaredAtoms(atoms: Atoms, toAdd: Settings, result: Settings,
                      var2Index: Map[Var[Dom], Int] = this.var2Index): Unit = {
    for (a <- atoms.cont; i <- var2Index.get(a.ownerOrSelf)) {
      val offset = a.offset
      val current = toAdd(i).cont(offset)
      result(i).cont(offset) += current * current
    }
    for (a <- atoms.vect; i <- var2Index.get(a.ownerOrSelf)) {
      val offset = a.offset
      val current = toAdd(i).vect(offset)
      val targetVector = result(i).vect(offset)
      for (j <- current.activeDomain) {
        targetVector(j) += current(j) * current(j)
      }
    }
    for (a <- atoms.mats; i <- var2Index.get(a.ownerOrSelf)) {
      val offset = a.offset
      val current = toAdd(i).mats(offset)
      val targetMatrix = result(i).mats(offset)
      //todo: probably slow
      for (i1 <- 0 until current.dim1; i2 <- 0 until current.dim2)
        targetMatrix(i1, i2) += current(i1, i2) * current(i1, i2)
    }
  }

  def gradientStep(epoch: Int, atoms: Atoms, gradient: Settings, momentum: Settings, result: Settings,
                   lambda: Double, var2Index: Map[Var[Dom], Int] = this.var2Index): Unit = {
    for (a <- atoms.cont; i <- var2Index.get(a.ownerOrSelf) if epoch >= delays.getOrElse(a, 0)) {
      val offset = a.offset
      val g = gradient(i).cont(offset)
      val h = momentum(i).cont(offset)
      result(i).cont(offset) += lambda / (sqrt(h) + delta) * g
      a.projectValue(result(i))

    }
    for (a <- atoms.vect; i <- var2Index.get(a.ownerOrSelf) if epoch >= delays.getOrElse(a, 0)) {
      val offset = a.offset
      val g = gradient(i).vect(offset)
      val h = momentum(i).vect(offset)
      for (j <- g.activeDomain) {
        val oldValue = result(i).vect(offset)(j)
        result(i).vect(offset)(j) += lambda / (sqrt(h(j)) + delta) * g(j)
        val newValue = result(i).vect(offset)(j)
        val deltaValue = math.abs(oldValue) - math.abs(newValue)
      }
      a.projectValue(result(i))
    }
    for (a <- atoms.mats; i <- var2Index.get(a.ownerOrSelf) if epoch >= delays.getOrElse(a, 0)) {
      val offset = a.offset
      val g = gradient(i).mats(offset)
      val h = momentum(i).mats(offset)
      //todo: probably slow
      for (i1 <- 0 until g.dim1; i2 <- 0 until g.dim2)
        result(i).mats(offset)(i1, i2) += lambda / (sqrt(h(i1, i2)) + delta) * g(i1, i2)
      a.projectValue(result(i))
    }
  }


}
