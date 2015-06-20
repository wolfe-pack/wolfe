package ml.wolfe.term

import ml.wolfe.term.TermImplicits._

import scala.collection.mutable.ArrayBuffer

/**
 * @author sameer
 * @since 6/8/15.
 */
case class MapSeqTerm[D <: Dom, To <: Dom,  R <: Term[VarSeqDom[D]], Body <: Term[To]](range: R, variable: Var[D], body: Body)
  extends Term[VarSeqDom[To]] with NAry {
  override val domain: VarSeqDom[To] = new VarSeqDom(body.domain, range.domain.maxLength, range.domain.minLength)

  override def isStatic: Boolean = range.isStatic && body.isStatic

  override def vars: Seq[AnyVar] = body.vars ++ range.vars ++ Seq(variable)

  override def arguments: IndexedSeq[ArgumentType] = IndexedSeq(range,body)

  override def copy(args: IndexedSeq[ArgumentType]): Term[Dom] =
    MapSeqTerm[D,To,R,Body](args(0).asInstanceOf[R],variable, args(1).asInstanceOf[Body])

  override type ArgumentType = AnyTerm

  val this2body = VariableMapping(vars, body.vars)
  val this2range = VariableMapping(vars, range.vars)

  class Loop(termInputs: Settings) {
    val indexOfVar = body.vars.indexOf(variable)
    val varInput = variable.domain.createSetting()
    val bodyInput = body.createInputSettings()
    val rangeInput = range.createInputSettings()

    this2range.linkTargetsToSource(termInputs, rangeInput)
    this2body.linkTargetsToSource(termInputs, bodyInput)
    if (indexOfVar != -1) bodyInput(indexOfVar) = varInput

    val rangeEval = range.evaluatorImpl(rangeInput)

    def apply(execution:Execution)(procedure: => Unit): Unit = {
      rangeEval.eval()(execution)
      val length = rangeEval.output.disc(0)
      val elemLength = range.domain.elementDom.lengths
      var offset = Offsets(discOff = 1)
      for (i <- 0 until length) {
        //copy element into body input at variable index
        if (indexOfVar != -1) varInput shallowAssign(rangeEval.output, offset, elemLength)
        procedure
        //move to next slot in range
        offset += elemLength
      }
    }
  }

  class MapSeqTermEvaluator(in:Settings) extends AbstractEvaluator(in)  {
    val output = domain.createSetting()
    val loop = new Loop(in)
    val bodyEval = body.evaluatorImpl(loop.bodyInput)

    def eval()(implicit execution:Execution) = {
      val mapped = calculateMap(loop, bodyEval.eval(), domain.elementDom.toValue(bodyEval.output))
      domain.copyValue(mapped, output)
    }
  }

  def calculateMap[B](loop:Loop, eval: =>Unit, value: => B)(implicit execution:Execution): IndexedSeq[B] = {
    var result = new ArrayBuffer[B]()
    loop(execution) {
      eval
      result += value
    }
    result
  }

  override def evaluatorImpl(in: Settings) = new MapSeqTermEvaluator(in)
}
