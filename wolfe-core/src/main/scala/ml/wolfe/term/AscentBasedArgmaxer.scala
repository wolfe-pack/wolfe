package ml.wolfe.term

import ml.wolfe.util.ProgressBar

/**
 * @author riedel
 */
class AscentBasedArgmaxer(val obj: DoubleTerm,
                          val wrt: Seq[Var[Dom]],
                          val iterations: Int,
                          val learningRate: Double,
                          val initParams: Array[Setting]) extends Argmaxer {

  val obsVars            = obj.vars.filterNot(wrt.contains)
  //get differentiator
  val diff               = obj.differentiator(wrt)

  val initParameters = if (initParams.isEmpty) wrt.map(_.domain.createZeroSetting()).toArray else initParams

  val obs2full   = VariableMapping(obsVars, obj.vars)
  val param2full = VariableMapping(wrt, obj.vars)

  val scale        = new Setting(numCont = 1)
  val currentValue = new Setting(numCont = 1)

  def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting]) = {

    val bar = new ProgressBar(iterations, 100)
    bar.start()

    //initialize learning rate (affects gradient by changing the final upstream error signal)
    scale.cont(0) = learningRate
    //initialize with observation
    obs2full.copyForwardDeep(observed, result)
    //initialize parameters
    param2full.copyForwardDeep(initParameters, result)

    //now optimize
    for (iteration <- 0 until iterations) {
      diff.addGradientAndValue(result, scale, result, currentValue)
      bar(s"Obj: ${currentValue.cont(0)}", lineBreak = true)
    }

  }
}
