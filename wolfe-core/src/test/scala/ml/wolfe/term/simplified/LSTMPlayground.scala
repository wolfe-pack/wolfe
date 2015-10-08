package ml.wolfe.term.simplified

/**
 * @author riedel
 */
object LSTMPlayground {

  def main(args: Array[String]) {

    import Wolfe._

    @term case class Cell(c: Vect, h: Vect)
    @term case class Theta(W_i: Mat, U_i: Mat, b_i: Vect,
                           W_c: Mat, U_c: Mat, b_c: Vect,
                           W_f: Mat, U_f: Mat, b_f: Vect,
                           W_o: Mat, U_o: Mat,
                           V_o: Mat, b_o: Vect,
                           init:Cell)

    def next(t: STerm[Theta])(last: STerm[Cell], x: STerm[Vect]) = {
      val i_t = sigmoid(t.W_i * x + t.U_i * last.h + t.b_i)
      val c_tilde = tanh(t.W_c * x + t.U_c * last.h + t.b_c)
      val f_t = sigmoid(t.W_f * x + t.U_f * last.h + t.b_f)
      val c = i_t * c_tilde + f_t * last.c
      val o_t = sigmoid(t.W_o * x + t.U_o * last.h + t.V_o * c + t.b_o)
      val h = o_t * tanh(c)
      Cell.Term(c, h)
    }

    val x = Var[Seq[Vect]]
    val theta = Var[Theta]

    val model = x.foldLeft(theta.init)(next(theta))


  }

}
