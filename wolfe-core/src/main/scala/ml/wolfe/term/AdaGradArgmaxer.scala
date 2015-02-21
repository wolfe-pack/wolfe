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
                      val delta: Double,
                      val initParams: Array[Setting],
                      val epochHook: (IndexedSeq[Any],Int) => String = null,
                      val adaptiveVectors: Boolean = true) extends Argmaxer {

  val obsVars = obj.vars.filterNot(wrt.contains)
  //get differentiator
  val diff = obj.differentiator(wrt)

  val initParameters = if (initParams.isEmpty) wrt.map(_.domain.createZeroSetting()).toArray else initParams

  val obs2full = VariableMapping(obsVars, obj.vars)
  val param2full = VariableMapping(wrt, obj.vars)

  val scale = new Setting(numCont = 1)
  val currentValue = new Setting(numCont = 1)

  val gradient = wrt.map(_.domain.createZeroSetting()).toArray
  val momentum = wrt.map(_.domain.createZeroSetting()).toArray

  val var2Index = wrt.map(v => v -> obj.vars.indexOf(v)).toMap

  val termsPerEpoch = obj match {
    case t: DynamicTerm[_, _] => t.generator.size
    case _ => 1
  }

  val epochs = iterations / termsPerEpoch
  var objAccumulator = 0.0

  if (adaptiveVectors) gradient.foreach(_.setAdaptiveVectors(true))

  def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting]) = {
    val bar = new ProgressBar(epochs, if (epochs < 100) 1 else epochs / 100)
    bar.start()

    //initialize learning rate (affects gradient by changing the final upstream error signal)
    scale.cont(0) = 1.0
    //initialize with observation
    obs2full.copyForwardDeep(observed, result)
    //initialize parameters
    param2full.copyForwardDeep(initParameters, result)

    //now optimize
    for (iteration <- 0 until iterations) {
      //reset all previous changes to the gradient
      if (iteration > 0) {
        val prevAtoms = Atoms.fromIterator(obj.atomsIterator)
        setAtoms(prevAtoms, gradient, var2Index, 0.0)
      }

      //add term gradient into result gradient
      diff.addGradientAndValue(result, scale, gradient, currentValue)

      //now update the momentum, need to call atoms again because atoms may have changed if objective is dynamic
      val currentAtoms = Atoms.fromIterator(obj.atomsIterator)
      addSquaredAtoms(currentAtoms, gradient, momentum, var2Index)

      //now add gradient into result parameters, using momentum to determine learning rate.
      gradientStep(currentAtoms, gradient, momentum, result, learningRate, var2Index)

      objAccumulator += currentValue.cont(0)
      if ((iteration + 1) % termsPerEpoch == 0) {
        if (epochHook != null) {
          val parameters = for ((v,s) <- wrt zip result) yield v.domain.toValue(s)
          val text = epochHook(parameters.toIndexedSeq,(iteration + 1) / termsPerEpoch)
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

  def addSquaredAtoms(atoms: Atoms, toAdd: Array[Setting], result: Array[Setting],
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

  def gradientStep(atoms: Atoms, gradient: Array[Setting], momentum: Array[Setting], result: Array[Setting],
                   lambda: Double, var2Index: Map[Var[Dom], Int] = this.var2Index): Unit = {
    for (a <- atoms.cont; i <- var2Index.get(a.ownerOrSelf)) {
      val offset = a.offset
      val g = gradient(i).cont(offset)
      val h = momentum(i).cont(offset)
      result(i).cont(offset) += lambda / (sqrt(h) + delta) * g
    }
    for (a <- atoms.vect; i <- var2Index.get(a.ownerOrSelf)) {
      val offset = a.offset
      val g = gradient(i).vect(offset)
      val h = momentum(i).vect(offset)
      for (j <- g.activeDomain) {
        val oldValue = result(i).vect(offset)(j)
        result(i).vect(offset)(j) += lambda / (sqrt(h(j)) + delta) * g(j)
        val newValue = result(i).vect(offset)(j)
        val deltaValue = math.abs(oldValue) - math.abs(newValue)
      }
    }
    for (a <- atoms.mats; i <- var2Index.get(a.ownerOrSelf)) {
      val offset = a.offset
      val g = gradient(i).mats(offset)
      val h = momentum(i).mats(offset)
      //todo: probably slow
      for (i1 <- 0 until g.dim1; i2 <- 0 until g.dim2)
        result(i).mats(offset)(i1, i2) += lambda / (sqrt(h(i1, i2)) + delta) * g(i1, i2)
    }
  }


}
