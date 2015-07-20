package ml.wolfe.term


case class MatrixApply(mat: MatrixTerm, i: IntTerm, j: IntTerm) extends ComposedDoubleTerm {
  type ArgumentType = AnyTerm

  val arguments = IndexedSeq(mat, i, j)

  def copy(args: IndexedSeq[ArgumentType]) =
    new MatrixApply(args(0).asInstanceOf[MatrixTerm],
      args(1).asInstanceOf[IntTerm],
      args(2).asInstanceOf[IntTerm]
    )

  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      val i = input(1).disc(0)
      val j = input(2).disc(0)
      output.cont(0) = input(0).mats(0)(i,j)
    }
  }
}








