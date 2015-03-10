package ml.wolfe.term

/**
 * @author riedel
 */
case class FoldLeft[D <: Dom,R <:Dom](collection:Term[VarSeqDom[D]],init:Term[R],op:(Term[R],Term[D]) => Term[R]) {


}
