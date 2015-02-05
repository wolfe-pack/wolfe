package ml.wolfe.term

import ml.wolfe.util.ProgressBar
import math._

/**
 * @author riedel
 */
class AdaGradArgmaxer(val obj: DoubleTerm,
                      val wrt: Seq[Var[Dom]],
                      val iterations: Int,
                      val learningRate: Double,
                      val initParams: Array[Setting]) extends Argmaxer {

  val obsVars = obj.vars.filterNot(wrt.contains)
  //get differentiator
  val diff    = obj.differentiator(wrt)

  val initParameters = if (initParams.isEmpty) wrt.map(_.domain.createZeroSetting()).toArray else initParams

  val obs2full   = VariableMapping(obsVars, obj.vars)
  val param2full = VariableMapping(wrt, obj.vars)

  val scale        = new Setting(numCont = 1)
  val currentValue = new Setting(numCont = 1)

  val gradient = wrt.map(_.domain.createZeroSetting()).toArray
  val momentum = wrt.map(_.domain.createZeroSetting()).toArray

  val var2Index = wrt.map(v => v -> obj.vars.indexOf(v)).toMap

  def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting]) = {

    val bar = new ProgressBar(iterations, 10)
    bar.start()

    //initialize learning rate (affects gradient by changing the final upstream error signal)
    scale.cont(0) = 1.0
    //initialize with observation
    obs2full.copyForward(observed, result)
    //initialize parameters
    param2full.copyForward(initParameters, result)

    //now optimize
    for (iteration <- 0 until iterations) {
      //reset all previous changes to the gradient
      val prevAtoms = obj.atoms
      setAtoms(prevAtoms, gradient, var2Index, 0.0)

      //add term gradient into result gradient
      diff.addGradientAndValue(result, scale, gradient, currentValue)

      //now update the momentum, need to call atoms again because atoms may have changed if objective is dynamic
      val currentAtoms = obj.atoms
      addSquaredAtoms(currentAtoms, gradient, momentum, var2Index)

      //now add gradient into result parameters, using momentum to determine learning rate.
      gradientStep(currentAtoms, gradient, momentum, result, learningRate, var2Index)

      bar(s"Obj: ${currentValue.cont(0)}", true)
    }
  }

  def setAtoms(lastAtoms: Atoms, result: Array[Setting], var2Index: Map[Var[Dom], Int] = this.var2Index, value: Double = 0.0): Unit = {
    for (a <- lastAtoms.cont; i <- var2Index.get(a.owner)) {
      result(i).cont(a.offset) = value
    }
    for (a <- lastAtoms.vect; i <- var2Index.get(a.owner)) {
      result(i).vect(a.offset) := value
    }
    for (a <- lastAtoms.mats; i <- var2Index.get(a.owner)) {
      result(i).mats(a.offset) := value
    }
  }

  def addSquaredAtoms(atoms: Atoms, toAdd: Array[Setting], result: Array[Setting],
                      var2Index: Map[Var[Dom], Int] = this.var2Index): Unit = {
    for (a <- atoms.cont; i <- var2Index.get(a.owner)) {
      val current = toAdd(i).cont(a.offset)
      result(i).cont(a.offset) += current * current
    }
    for (a <- atoms.vect; i <- var2Index.get(a.owner)) {
      val current = toAdd(i).vect(a.offset)
      val targetVector = result(i).vect(a.offset)
      for (j <- current.activeDomain) {
        targetVector(j) += current(j) * current(j)
      }
    }
    for (a <- atoms.mats; i <- var2Index.get(a.owner)) {
      val current = toAdd(i).mats(a.offset)
      val targetMatrix = result(i).mats(a.offset)
      //todo: probably slow
      for (i1 <- 0 until current.dim1; i2 <- 0 until current.dim2)
        targetMatrix(i1, i2) += current(i1, i2)
    }
  }

  def gradientStep(atoms: Atoms, gradient: Array[Setting], momentum: Array[Setting], result: Array[Setting],
                   lambda: Double, var2Index: Map[Var[Dom], Int] = this.var2Index): Unit = {
    for (a <- atoms.cont; i <- var2Index.get(a.owner)) {
      val g = gradient(i).cont(a.offset)
      val h = momentum(i).cont(a.offset)
      result(i).cont(a.offset) += lambda / sqrt(h) * g
    }
    for (a <- atoms.vect; i <- var2Index.get(a.owner)) {
      val g = gradient(i).vect(a.offset)
      val h = momentum(i).vect(a.offset)
      for (j <- g.activeDomain) {
        result(i).vect(a.offset)(j) += lambda / sqrt(h(j)) * g(j)
      }
    }
    for (a <- atoms.mats; i <- var2Index.get(a.owner)) {
      val g = gradient(i).mats(a.offset)
      val h = momentum(i).mats(a.offset)
      //todo: probably slow
      for (i1 <- 0 until g.dim1; i2 <- 0 until g.dim2)
        result(i).mats(a.offset)(i1, i2) += lambda / sqrt(h(i1, i2)) * g(i1, i2)
    }
  }


}
