package ml.wolfe.fg20

import ml.wolfe.fg20

/**
 * @author Sebastian Riedel
 */
class DualDecomposition {

}

object DualDecomposition {

  trait Potential extends fg20.Potential {type Proc <: ArgmaxProcessor }

}

trait ArgmaxProcessor extends fg20.Scorer {
  def argmax(obs: PartialSetting, incoming: Msgs, result: Setting): Double
}


