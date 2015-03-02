package ml.wolfe.term

/**
 * @author riedel
 */
trait TermMap[A <: Term[Dom], D <: Dom] extends Term[D] {
  val term:A
  val domain:D
  def f(arg:term.domain.Value):domain.Value

  def vars = term.vars

  def atomsIterator = term.atomsIterator

  def evaluator() = new Evaluator {
    val termEval = term.evaluator()
    val termOutput = term.domain.createSetting()
    def eval(inputs: Array[Setting], output: Setting) = {
      termEval.eval(inputs,termOutput)
      val value = term.domain.toValue(termOutput)
      val mapped = f(value)
      domain.copyValue(mapped,output)
    }
  }


  override def evaluatorImpl(in: Settings) = new AbstractEvaluator2(in) {
    val termEval = term.evaluatorImpl(in)
    var currentExecution:Execution = null
    def eval()(implicit execution: Execution): Unit = {
      if (currentExecution != execution) {
        termEval.eval()
        val value = term.domain.toValue(termEval.output)
        val mapped = f(value)
        domain.copyValue(mapped, output)
        currentExecution = execution
      }
    }
    val output = domain.createSetting()
  }

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}

trait TermFlatMap[A <: Term[Dom], D <: Dom] extends Term[D] {
  val term:A
  val domain:D
  def f(arg:term.domain.Value):Term[D]

  lazy val prototype = f(term.domain.zero)

  def vars = (term.vars ++ prototype.vars).distinct
  def atomsIterator = term.atomsIterator ++ prototype.atomsIterator

  def evaluator() = new Evaluator {
    //need to map
    val this2term = VariableMapping(vars,term.vars)
    val this2proto = VariableMapping(vars,prototype.vars)
    val termInputs = term.vars.map(_.domain.createSetting()).toArray
    val protoInputs = prototype.vars.map(_.domain.createSetting()).toArray
    val termEval = term.evaluator()
    val termOutput = term.domain.createSetting()
    def eval(inputs: Array[Setting], output: Setting) = {
      this2term.copyForwardShallow(inputs,termInputs)
      this2proto.copyForwardShallow(inputs,protoInputs)
      termEval.eval(termInputs,termOutput)
      val value = term.domain.toValue(termOutput)
      val mapped = f(value)
      mapped.evaluator().eval(protoInputs,output)
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = ???


}

object TermMonadTest {



  def main(args: Array[String]) {
    import TermImplicits._
    val x = bools.variable("x")
    val z = bools.variable("z")
    val y = for (i <- x) yield !i
    val t = for (i <- x; j <- z) yield !i || j
    println(y.eval(true))
    println(y.eval(true,false))

  }
}