package ml.wolfe.term

import java.util

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
        result shallowAssign innerEval.output
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

/**
 * Caching values based on input arguments.
 * @author riedel
 */
class CachedPotential[T <: DoubleTerm](keys:AnyTerm*)(val term:T) extends NAry with Term[TypedDom[Double]] {
  val domain:term.domain.type = term.domain
  val vars = ((keys flatMap (_.vars)) ++ term.vars).distinct

  require(keys.forall(_.domain.isDiscrete))

  def isStatic = term.isStatic

  private val keyCount = keys.map(_.domain.domainSize).product
  private val key2Value = Array.ofDim[Double](keyCount)
  private val key2Set = Array.ofDim[Boolean](keyCount)

  private val inputs2Value = new mutable.HashMap[IndexedSeq[Int],Setting]

  def clearCache(): Unit = {
    util.Arrays.fill(key2Set,false)
  }



  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {

    val indexedVars = vars.toIndexedSeq
    val innerEval = term.evaluatorImpl(in.linkedSettings(vars,term.vars))
    val output = domain.createSetting()
    val keyEvals = keys.toIndexedSeq map ( k =>  k -> k.evaluatorImpl(in.linkedSettings(vars,k.vars)))

    def currentKey()(implicit execution: Execution) = {
      var result = 0
      for (k <- keyEvals) {
        k._2.eval()
        val index = k._1.domain.indexOfSetting(k._2.output)
        result = index + result * k._1.domain.domainSize
      }
      result
    }

    def eval()(implicit execution:Execution) = {
      val key = currentKey()
      if (key2Set(key)) output.cont(0) = key2Value(key) else {
        innerEval.eval()
        val value = innerEval.output.cont(0)
        key2Value(key) = value
        key2Set(key) = true
        output.cont(0) = value
      }
    }
  }



  type ArgumentType = AnyTerm

  val arguments = keys.toIndexedSeq :+ term

  def copy(args: IndexedSeq[ArgumentType]) = new CachedPotential[T](args.dropRight(1):_*)(args.last.asInstanceOf[T])

  override def toString = s"cached($term)"
}
