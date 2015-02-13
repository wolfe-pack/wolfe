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

  val termsPerEpoch = obj match {
    case t: DynamicTerm[_, _] => t.generator.termsPerEpoch
    case _ => 1
  }
  val epochs = iterations / termsPerEpoch
  var objAccumulator = 0.0

  def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting]) = {
    val bar = new ProgressBar(epochs, if (epochs < 100) 1 else epochs / 100)
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
      objAccumulator += currentValue.cont(0)
      if ((iteration + 1) % termsPerEpoch == 0) {
        bar(s"Obj: $objAccumulator", lineBreak = true)
        objAccumulator = 0.0
      }
    }

  }
}
