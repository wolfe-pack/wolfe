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

    @term case class Cell(c: Vect, h: Vect)
    @term case class Theta(W: Mat, b_i: Vect, b_c: Vect, b_f: Vect, b_o: Vect, V_o: Mat, init:Cell)

    def concat(arg1: STerm[Vect], arg2: STerm[Vect]): STerm[Vect] = ???
    def select(arg1: STerm[Vect], arg2: Int): STerm[Vect] = ???

    def next(t: STerm[Theta])(last: STerm[Cell], x: STerm[Vect]) = {
      val xh = concat(x, last.h)

      val M: STerm[Vect] = t.W * xh

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

    val xs = Var[Seq[Vect]]
    val theta = Var[Theta]

    //if you only need the last output representation
    val last: STerm[Cell] = xs.foldLeft(theta.init)(next(theta))

    //if you need all output vectors
    val mapped: STerm[Seq[Cell]] = xs.scanLeft(theta.init)(next(theta))
  }
}