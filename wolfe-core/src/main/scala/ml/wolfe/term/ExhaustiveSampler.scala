package ml.wolfe.term

import ml.wolfe.term

import scala.util.Random


class ExhaustiveSampler(val obj:DoubleTerm, val wrt:Seq[AnyVar])(val observed:Settings, val msgs: Msgs)
                       (implicit val random:Random) extends Sampler {


  val obsVars = obj.vars.filterNot(wrt.contains)

  val wrt2obj = VariableMapping(wrt,obj.vars)
  val obs2obj = VariableMapping(obsVars,obj.vars)

  val result = Settings.fromSeq(wrt map (_.domain.createSetting()))
  val toVary = Settings.fromSeq(wrt map (_.domain.createSetting()))

  val objInput = obj.createInputSettings()

  wrt2obj.linkTargetsToSource(toVary,objInput)
  obs2obj.linkTargetsToSource(observed,objInput)

  val objEval = obj.evaluatorImpl(objInput)

  val allSettings = new term.AllSettings(wrt.map(_.domain).toIndexedSeq, toVary)(_ => {})

  /*

    assert(normalizer > 0.0, "normalizer = "+normalizer)
    val l = length; var b = 0.0; val s = r.nextDouble * normalizer; var i = 0
    while (b <= s && i < l) { assert(apply(i) >= 0.0); b += apply(i); i += 1 }
    assert(i > 0)
    i - 1

   */

  def sample()(implicit execution: Execution) = {
    var normalizer = 0.0

    allSettings.loopSettings { settings =>
      objEval.eval()
      normalizer += math.exp(objEval.output.cont(0))
    }

    val sampled = random.nextDouble() * normalizer

    var current = 0.0
    allSettings.loopWhile { settings =>
      objEval.eval()
      current += math.exp(objEval.output.cont(0))
      val continue = current <= sampled
      if (!continue) result := settings
      continue
    }
  }
}


