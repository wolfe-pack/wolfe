package ml.wolfe.term

import java.util

import scala.collection.mutable

/**
 * @author riedel
 */
case class PTree(parse: Term[VarSeqDom[VarSeqDom[BooleanDom]]], slen: IntTerm) extends ComposedDoubleTerm {

  def copy(args: IndexedSeq[ArgumentType]) = ???

  val arguments = IndexedSeq(parse, slen)

  type ArgumentType = AnyTerm

  val parseDom = parse.domain

  //todo: this accessor should come from the domain of parse
  def edge(parse: Array[Int])(head: Int, mod: Int) = parse(1 + mod * parseDom.elementDom.lengths.discOff + head + 1)

  def maxNumHeads(parse: Array[Int])(mod: Int) = parse(1 + mod * parseDom.elementDom.lengths.discOff)

  override def composer(args: Settings) = new Composer(args) {

    var heads: Array[Int] = null
    var visited: Array[Boolean] = null

    def eval()(implicit execution: Execution): Unit = {
      output.cont(0) = Double.NegativeInfinity
      val parse = input(0).disc.array
      val expectedLength = input(1).disc(0)

      //check whether all lengths are correct
      val actualLength = parse(0)
      if (actualLength != expectedLength) return
      for (mod <- 0 until actualLength) if (maxNumHeads(parse)(mod) != expectedLength) return

      //println("correct length")

      //instantiate head array
      if (heads == null || heads.length < actualLength) {
        heads = Array.ofDim[Int](actualLength)
        visited = Array.ofDim[Boolean](actualLength)
      }
      util.Arrays.fill(visited, false)
      util.Arrays.fill(heads, -1)
      //check whether exactly one token has no head, and all other tokens have exactly one head
      //and set up head pointers
      var tokensWithHeads = 0
      var root = -1
      for (mod <- 0 until actualLength) {
        heads(mod) = -1
        for (head <- 0 until actualLength) {
          if (edge(parse)(head, mod) == 1) {
            if (heads(mod) != -1) return
            heads(mod) = head
            tokensWithHeads += 1
          }
        }
        if (heads(mod) == -1) root = mod
      }
      if (tokensWithHeads != actualLength - 1) return
      //println("right number of heads")


      //test whether there are loops
      for (mod <- 0 until actualLength; if !visited(mod)) {
        var current = mod
        val path = new mutable.HashSet[Int]
        do {
          if (path(current)) return //cycle
          visited(current) = true
          path += current
          current = heads(current)
        } while (current != -1)
      }
      //println("no loops")

      //test whether there are crossing edges
      //todo: quadratic right now, should be possible in linear fashion
      for (m1 <- 0 until actualLength; if m1 != root) {
        for (m2 <- 0 until actualLength; if m2 != root && m2 != m1) {
          val h1 = heads(m1)
          val h2 = heads(m2)

          val (l1, r1) = if (h1 < m1) (h1, m1) else (m1, h1)
          val (l2, r2) = if (h2 < m2) (h2, m2) else (m2, h2)

          if (l1 < l2 && r1 > l2 && r2 > r1 || l1 > l2 && l1 < r2 && r2 < r1) return //detected crossing edges
        }
      }
      //println("no crossing edges")


      output.cont(0) = 0.0


    }
  }

  override def maxMarginalizerImpl(wrt: Seq[AnyVar], observed: Seq[AnyVar])
                                  (input: Settings, inputMsgs: Msgs, reverseMsgsAlso: Boolean) = {
    new MaxMarginalizer {
      //require that the parse variable term is a sequence of sequence of boolean vars
      //require that wrt contains all variable (we don't do partial msgs right now
      //require that reverseMsgAlso is true (we want to get msgs on target vars)

      //require(parse.isInstanceOf[AnyVar])
      def maxMarginals()(implicit execution: Execution) = ???

      val input: Settings = ???
      val outputMsgs: Msgs = ???
      val inputMsgs: Msgs = ???
    }
  }
}
