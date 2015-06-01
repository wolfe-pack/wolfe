package ml.wolfe.term

/**
 * @author riedel
 */
trait GenericDiscreteDom[A <: Any] extends AtomicDom {

  dom =>
  type Value = A
  type Var = DomVar
  type Marginals = Map[A,Double]


  def start = valueToInt(zero)
  def end = start + domainSize

  def intToValue(int: Int): Value

  def valueToInt(value: Value): Int

  def domainSize: Int

  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    intToValue(setting.disc(offsets.discOff))


  def domainValues = for (i <- start until end) yield intToValue(i)

  def toMarginals(msg: Msg, offsets: Offsets) = {
    val pairs = for (i <- start until end) yield intToValue(i) -> msg.disc(offsets.discOff).msg(i - start)
    pairs.toMap
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.disc(offsets.discOff) = valueToInt(value)


  def copyMarginals(marginals: Marginals, msgs: Msg, offsets: Offsets) = {
    val start = valueToInt(zero)
    val end = start + domainSize
    val margs = domainValues map (v => marginals(v))
    msgs.disc(offsets.discOff) = new DiscMsg(margs.toArray)
  }

  def fillZeroMsg(target: Msg, offsets: Offsets) = {
    target.disc(offsets.discOff) = new DiscMsg(domainSize)
  }

  val lengths = Offsets(1, 0, 0, 0)

  def Variable(name: String) = new StaticDiscVar(name)

  class StaticDiscVar(val varName: String) extends DomVar


}
