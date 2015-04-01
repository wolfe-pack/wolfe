package ml.wolfe.nlp

/**
 * Class to represent coreference mention
 * @param clusterID ID of the cluster (chain) of connected mentions
 * @param sentence sentence index
 * @param start starting index
 * @param end ending index (points to the first token AFTER the mention)
 * @param head head of the coreference mention
 */
case class CorefMention(clusterID: Int, sentence: Int, start: Int, end: Int, head: Int = -1) extends Ordered[CorefMention] {

  // symmetric nest
  def areNested(that: CorefMention): Boolean = {
    this.nests(that) || that.nests(this)
  }

  // asymmetric -- If m1 nests m2
  def nests(that: CorefMention): Boolean = {
    this.sentence == that.sentence && this.start <= that.start && this.end >= that.end && this != that
  }

  def crosses(that: CorefMention): Boolean = {
    this.sentence == that.sentence &&
      ((this.start < that.start && this.end > that.start && this.end < that.end) ||
        (this.start > that.start && this.start < that.end && this.end > that.end))
  }

  override def compare(that: CorefMention): Int = {
    if (this.sentence < that.sentence) return -1
    else if (this.sentence > that.sentence) return 1
    else {
      if (this.start < that.start) return -1
      else if (this.start > that.start) return 1
      else {
        if (this.end < that.end) return -1
        else if (this.end > that.end) return 1
        else return 0
      }
    }
  }

  override def toString = "[Coref Mention: Sent: %d, Start: %d, End: %d, Cluster: %d]".format(sentence, start, end, clusterID)

/*
  override def toString = {
    "[COREF MENTION\n" +
    "  START: %d\n".format(start) +
    "  END: %d\n".format(end) +
      (if (head >= 0) "  HEAD: %d\n".format(head) else "") +
    "  SENTENCE: %d\n".format(sentence) +
    "  CLUSTER: %d]\n".format(clusterID)
  }
*/

  def width = end - start
}
