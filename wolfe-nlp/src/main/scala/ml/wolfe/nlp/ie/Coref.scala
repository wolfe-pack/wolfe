package ml.wolfe.nlp.ie

/**
 * Created by narad on 3/20/15.
 */

import ml.wolfe.nlp.{Document, Token}

import scala.collection.mutable.HashMap
/**
 * Class to represent coreference annotation
 * @param mentions sequence of CorefMention elements
 */
case class CorefAnnotation(mentions: Seq[CorefMention] = Seq.empty) {

  def clusterOf(s: Int, i: Int, j: Int): Option[Int] = {
    mentions.find(m => m.sentence == s && m.start == i && m.end == j) match {
      case Some(x) => Some(x.clusterID)
      case _ => None
    }
  }

  def distanceInMentions(m1: CorefMention, m2: CorefMention): Int = {
    mentions.count(m => m1 < m && m < m2)
  }

  def hasMention(s: Int, i: Int, j: Int): Boolean = {
    mentions.exists(m => m.sentence == s && m.start == i && m.end == j)
  }

  def mentionTokens(m: CorefMention, d: Document): IndexedSeq[Token] = {
    assert(m.sentence < d.sentences.size, "Document does not have a sentence at idx = %d.".format(m.sentence))
    assert(d.sentences(m.sentence).size >= m.end, "Sentence at idx = %d is of len %d (queried mention ends at %d).".format(m.sentence, m.end))
    d.sentences(m.sentence).tokens.slice(m.start, m.end)
  }

  def mentionTokens(m: (Int,Int,Int), d: Document): IndexedSeq[Token] = {
    mentionTokens(CorefMention(sentence = m._1, start = m._2, end = m._3, clusterID = -1), d)
  }

  def shareCluster(m1: CorefMention, m2: CorefMention): Boolean = {
    shareCluster(m1.sentence, m1.start, m1.end, m2.sentence, m2.start, m2.end)
  }

  def shareCluster(s1: Int, i1: Int, j1: Int, s2: Int, i2: Int, j2: Int): Boolean = {
    if (askedBefore(s1, i1, j1, s2, i2, j2)) return cachedAnswer((s1, i1, j1, s2, i2, j2))
    val answer = clusterOf(s1, i1, j1) == clusterOf(s2, i2, j2)
    cachedAnswer((s1, i1, j1, s2, i2, j2)) = answer
    answer
  }

  val cachedAnswer = new HashMap[(Int,Int,Int,Int,Int,Int), Boolean]

  def askedBefore(s1: Int, i1: Int, j1: Int, s2: Int, i2: Int, j2: Int) = cachedAnswer.contains((s1, i1, j1, s2, i2, j2))
}

object CorefAnnotation {
  val empty = CorefAnnotation()

}

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