package ml.wolfe.term

import scala.collection.mutable

/**
 * Caching values based on input arguments.
 * @author riedel
 */
case class CachedTerm[D <: Dom, T <: Term[D]](keys:AnyTerm*)(val term:T) extends NAry with Term[D] {
  val domain:term.domain.type = term.domain
  val vars = ((keys flatMap (_.vars)) ++ term.vars).distinct

  def isStatic = term.isStatic

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {

    val indexedVars = vars.toIndexedSeq
    val innerEval = term.evaluatorImpl(in.linkedSettings(vars,term.vars))
    val output = domain.createSetting()
    val keyEvals = keys.toIndexedSeq map ( k =>  k -> k.evaluatorImpl(in.linkedSettings(vars,k.vars)))

    private val inputs2Value = new mutable.HashMap[IndexedSeq[Int],Setting]

    def eval()(implicit execution:Execution) = {
      val indices = keyEvals map (k => {
        k._2.eval()
        k._1.domain.indexOfSetting(k._2.output)
      })
      val cached = inputs2Value.getOrElseUpdate(indices, {
        innerEval.eval()
        val result = domain.createSetting()
        result := innerEval.output
        result
      })
      cached.shallowCopy(output)
    }

  }
  
  

  type ArgumentType = AnyTerm

  val arguments = keys.toIndexedSeq :+ term

  def copy(args: IndexedSeq[ArgumentType]) = new CachedTerm[D,T](args.dropRight(1):_*)(args.last.asInstanceOf[T])

  override def toString = s"cached($term)"
}
