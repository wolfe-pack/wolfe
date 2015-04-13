package ml.wolfe.term

import scala.collection.mutable

/**
 * Caching values based on input arguments.
 * @author riedel
 */
class CachedTerm[D <: Dom, T <: Term[D]](val term:T) extends NAry with Term[D] {
  val domain:term.domain.type = term.domain
  def vars = term.vars

  def isStatic = term.isStatic

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {

    val indexedVars = vars.toIndexedSeq
    val innerEval = term.evaluatorImpl(in)
    val output = domain.createSetting()

    private val inputs2Value = new mutable.HashMap[IndexedSeq[Int],Setting]

    def eval()(implicit execution:Execution) = {
      val indices = (0 until input.length) map (i => indexedVars(i).domain.indexOfSetting(input(i)))
      val cached = inputs2Value.getOrElseUpdate(indices, {
        innerEval.eval()
        val result = domain.createSetting()
        result := innerEval.output
        result
      })
      cached.shallowCopy(output)
    }

  }
  
  

  type ArgumentType = T

  def arguments = IndexedSeq(term)

  def copy(args: IndexedSeq[ArgumentType]) = new CachedTerm[D,T](args(0))

  override def toString = s"cached($term)"
}
