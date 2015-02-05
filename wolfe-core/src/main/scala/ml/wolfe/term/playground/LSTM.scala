package ml.wolfe.term.playground

import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe.util.Math._
/**
 * @author rockt
 */
object LSTM extends App {
  val T = 100

  val x = vectors(T).variable("x")
  val h0 = vectors(T).variable("h0")

  val Theta = seqs(matrices(T,T), 8)

  def lstm(x_t: VectorTerm, h_t1: VectorTerm, m_t1: VectorTerm)(W: Theta.Var):(VectorTerm, VectorTerm) = {
    val i_t = sigmVec(W(0) * x_t + W(1) * h_t1)
    val d_t = tanhVec(W(2) * x_t + W(3) * h_t1)
    val f_t = sigmVec(W(4) * x_t + W(5) * h_t1)
    val o_t = sigmVec(W(6) * x_t + W(7) * h_t1)

    val m_t = m_t1 :* f_t + i_t :* d_t
    val h_t = m_t :* o_t

    (m_t, h_t)
  }


}