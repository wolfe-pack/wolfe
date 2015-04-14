package ml.wolfe.term

import cc.factorie.la.DenseTensor1

/**
 * @author riedel
 */
case class Sum(arguments: IndexedSeq[DoubleTerm]) extends ComposedDoubleTerm {

  sum =>

  type ArgumentType = DoubleTerm

  def copy(args: IndexedSeq[ArgumentType]) = new Sum(args)

  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution): Unit = {
      output.cont(0) = 0.0
      for (i <- 0 until size) output.cont(0) += input(i).cont(0)
    }
  }

  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) = new Differentiator {


    val input = in
    val error = err
    val gradientAccumulator = gradientAcc

    val argDiffs = for (a <- arguments) yield
      a.differentiatorImpl(wrt)(input.linkedSettings(vars, a.vars), err, gradientAccumulator.linkedSettings(vars, a.vars))
    val argOutputs = Settings.fromSeq(argDiffs.map(_.output))
    val comp = composer(argOutputs)
    val output = comp.output

    def forward()(implicit execution: Execution) = {
      argDiffs foreach (_.forward())
      comp.eval()
    }

    def backward()(implicit execution: Execution) = {
      argDiffs foreach (_.backward())
    }

    def withRespectTo = wrt
  }
}

case class VarSeqSum[D <: TypedDom[Double], T <: Term[VarSeqDom[D]]](length: IntTerm,elements:Seq[DoubleTerm]) extends ComposedDoubleTerm {

  self =>

  type ArgumentType = Term[Dom]

  val arguments = length +: elements.toIndexedSeq

  override def toString = s"""sum($length)(${elements.mkString(", ")}})"""

  def copy(args: IndexedSeq[ArgumentType]) = VarSeqSum(args(0).asInstanceOf[IntTerm],args.drop(1).asInstanceOf[Seq[DoubleTerm]])


  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      val length = input(0).disc(0)
      output.cont(0) = 0.0
      for (i <- 0 until length) output.cont(0) += input(i+1).cont(0)
    }

    override def abort(index: Int) = {
      index > 0 && index > input(0).disc(0)
    }
  }

  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt,in,err,gradientAcc) {

      def localBackProp()(implicit execution: Execution) = {
        val length = argOutputs(0).disc(0)
        for (i <- 0 until length) {
          argErrors(i+1) := error
        }
      }

      override def abortForward(index: Int) = {
        index > 0 && index > argOutputs(0).disc(0)
      }

      override def abortBackward(index: Int) = abortForward(index)
    }



}

/**
 * @author riedel
 */
case class VectorSum(arguments: IndexedSeq[VectorTerm]) extends Composed[GenericVectorDom] {

  sum =>

  type ArgumentType = VectorTerm

  def copy(args: IndexedSeq[ArgumentType]) = new VectorSum(args)

  val domain = arguments.head.domain

  override def composer(args: Settings) = new Composer(args) {
    output.setAdaptiveVectors(true)
    def eval()(implicit execution: Execution): Unit = {
      if (output.vect(0) != null) output.vect(0).zero()
      for (i <- 0 until size) output.vect.add(0,input(i).vect(0))
//      println("Input:\n" + input.map(_.vect(0)).mkString("\n"))
//      println("Output:\n" + output.vect(0))
    }
  }


}



