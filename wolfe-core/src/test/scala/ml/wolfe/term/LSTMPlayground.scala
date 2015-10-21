package ml.wolfe.term

import ml.wolfe.term._

/**
 * @author riedel
 */
/*
object LSTMPlayground {

  def main(args: Array[String]) {

    import Wolfe._

    @term case class Cell(c: Tensor, h: Tensor)
    @term case class Theta(W_i: Tensor, U_i: Tensor, b_i: Tensor,
                           W_c: Tensor, U_c: Tensor, b_c: Tensor,
                           W_f: Tensor, U_f: Tensor, b_f: Tensor,
                           W_o: Tensor, U_o: Tensor,
                           V_o: Tensor, b_o: Tensor,
                           init:Cell)

    def next(t: Term[Theta])(last: Term[Cell], x: Term[Tensor]) = {
      val i_t = sigmoid(t.W_i * x + t.U_i * last.h + t.b_i)
      val c_tilde = tanh(t.W_c * x + t.U_c * last.h + t.b_c)
      val f_t = sigmoid(t.W_f * x + t.U_f * last.h + t.b_f)
      val c = i_t * c_tilde + f_t * last.c
      val o_t = sigmoid(t.W_o * x + t.U_o * last.h + t.V_o * c + t.b_o)
      val h = o_t * tanh(c)
      Cell.Term(c, h)
    }

    val x = Var[Seq[Tensor]]
    val theta = Var[Theta]

    val model = x.foldLeft(theta.init)(next(theta))

    //pretending we have AD implementations
    //val diff = differentiator(model, wrt = theta)
    //val gradient = diff.gradient( x := someX, theta := someTheta)


  }

}

/**
 * @author rockt
 */
object LSTMPlayground2 {

  def main(args: Array[String]) {

    import Wolfe._

    @term case class Cell(c: Tensor, h: Tensor)
    @term case class Theta(W: Tensor, b_i: Tensor, b_c: Tensor, b_f: Tensor, b_o: Tensor, V_o: Tensor, init:Cell)

    def concat(arg1: Term[Tensor], arg2: Term[Tensor]): Term[Tensor] = ???
    def select(arg1: Term[Tensor], arg2: Int): Term[Tensor] = ???

    def next(t: Term[Theta])(last: Term[Cell], x: Term[Tensor]) = {
      val xh = concat(x, last.h)

      val M: Term[Tensor] = t.W * xh

      val M_i = select(M, 1)
      val M_c = select(M, 2)
      val M_f = select(M, 3)
      val M_o = select(M, 4)

      val i_t = sigmoid(M_i + t.b_i)
      val c_tilde = tanh(M_c + t.b_c)
      val f_t = sigmoid(M_f + t.b_f)
      val c = i_t * c_tilde + f_t * last.c
      val o_t = sigmoid(M_o + t.V_o * c + t.b_o)
      val h = o_t * tanh(c)
      Cell.Term(c, h)
    }

    val xs = Var[Seq[Tensor]]
    val theta = Var[Theta]

    //if you only need the last output representation
    val last: Term[Cell] = xs.foldLeft(theta.init)(next(theta))

    //if you need all output vectors
    val mapped: Term[Seq[Cell]] = xs.scanLeft(theta.init)(next(theta))
  }
}
*/