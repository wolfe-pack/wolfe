package ml.wolfe.nlp.syntax

/**
 * Created by narad on 3/19/15.
 */
case class ConstituentSpan(left: Int, right: Int, label: String,
                           var height: Int = 0, headInfo: Option[HeadInfo] = None) {

  def width: Int = right - left

  def covers(other: ConstituentSpan): Boolean = {
    left <= other.left && right >= other.right && !equals(other)
  }

  def crosses(other: ConstituentSpan): Boolean = {
    (start < other.start && end > other.start   && end < other.end) ||
      (start > other.start && start < other.end && end > other.end)
  }

  override def equals(that: Any): Boolean = that match {
    case other: ConstituentSpan => left == other.left && right == other.right && other.label == label
    case _=> false
  }

  def isUnary = height > 0

  def start = left

  def end = right

  def isTerminal = !isUnary && width == 1

  override def toString(): String = "%s(%s,%s,%d)".format(label, left, right, height)
}