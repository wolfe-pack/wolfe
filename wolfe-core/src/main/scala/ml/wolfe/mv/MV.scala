package ml.wolfe.mv

/**
 * A multivariate expression for which the value is defined through a set of
 * integer and real variables.
 * @author Sebastian Riedel
 */
trait MV[T] {
  def value(): T
  def vars: List[Var[_]]
  def allValues: Iterator[T] = Var.allStates(vars).map(_ => value())
}


object MV {
  def bool() = new MVMapped(new IntVar(2), Array(false, true))
  def double() = new DoubleVar(Double.NegativeInfinity, Double.PositiveInfinity)
  def dom[T](values: Seq[T]) = new MVMapped(new IntVar(values.size), values)
  def tuple[T1,T2,M1<%MV[T1],M2 <%MV[T2]](m1:M1,m2:M2) = new MVTuple2(m1,m2)
  def seq[T,M<%MV[T]](length: Int, dom: => M) = new MVSeq(IndexedSeq.fill(length)(dom))
  def seq[T,M<%MV[T]](mvs:Seq[M]) = new MVSeq(mvs)

}

sealed trait Var[T] extends MV[T] {
  def current: T
  def vars = List(this)
  def value() = current
}

object Var {
  def allStates(vars: List[Var[_]], result: => Iterator[Unit] = Iterator(())): Iterator[Unit] = {
    vars match {
      case Nil => result
      case head :: tail => head match {
        case d:IntVar =>
          def discreteIterator = Range(0, d.max).iterator.map(d.current = _)
          def combined = for (d <- discreteIterator; s <- result) yield ()
          allStates(tail, combined)
        case _ => sys.error("Can only iterate over int variables")
      }
    }
  }
}

class IntVar(val max: Int) extends Var[Int] {
  var current = 0
}
class DoubleVar(val min: Double, val max: Double) extends Var[Double] {
  var current = 0.0
}

class MVMapped[T, S, M <% MV[T]](val source: M, f: T => S) extends MV[S] {
  def value() = f(source.value())
  def vars = source.vars
}

class MVConstant[T](_value:T) extends MV[T] {
  def value() = _value
  def vars = Nil
}

class MVTuple2[T1, T2, M1 <% MV[T1], M2 <% MV[T2]](val _1: M1, val _2: M2) extends MV[(T1, T2)] {
  def value() = (_1.value(), _2.value())
  def vars = _1.vars ::: _2.vars
}

class MVSeq[T,M<%MV[T]](val seq:Seq[M]) extends MV[Seq[T]] {
  def value() = seq map (_.value())
  def vars = seq.toList flatMap (_.vars)
  def apply(index:Int) = seq(index)
}

class Times(arg1:MV[Double],arg2:MV[Double]) extends MV[Double] {
  def value() = arg1.value() * arg2.value()
  def vars = arg1.vars ::: arg2.vars
}

object GPLVMTest {

  implicit class RichMV[T, M <% MV[T]](mv:M) {
    def map[S](f: T => S) = new MVMapped(mv, f)
    def flatMap[S](f: T => MV[S]): MVMapped[Int, S, MV[Int]] = {
      val domain = mv.allValues.flatMap(t => f(t).allValues).toSeq
      val variable = new IntVar(domain.size)
      new MVMapped(variable, domain)
    }
  }

  implicit class RichDoubleMV(mv:MV[Double]) {
    def *(that:MV[Double]) = new Times(mv,that)
  }

//  def prod(args:Seq[MV[Double]]) = args.foldLeft(new MVConstant(1.0)) { new Times(_,_) }


  class GPLVMGaussian(y:Seq[Double],latent:Seq[Seq[DoubleVar]]) extends MV[Double] {
    def value() = 0.0
    def vars = for (vars <- latent.toList; v <- vars) yield v
  }



  def main(args: Array[String]) {
    //create a data structure that represents items using latent features
    case class Item(name: String, features: Seq[Double])

    //create a mv representation for a specific item
    def item(index: Int) = MV.seq(5, MV.double()) map (Item("item " + index, _))

    //a multivariate representations of a set of items
    val items = MV.seq(Range(0,5) map item)

    //create a single factor
    val f1 = new GPLVMGaussian(Seq(2.0,5.0),Seq(0,1) map items.seq map (_.source.seq))
    val f2 = new GPLVMGaussian(Seq(2.0,3.0),Seq(0,2) map items.seq map (_.source.seq))

    val tuple = MV.tuple(items(0).source(0),items(0).source(1)) //map { pair => 0.0 }

    //generic way
    val f3 = tuple map ((pair:(Double,Double)) => pair._1 + pair._2)

    //model is a product of factors
    val model = f1 * f2 * f3

    //create the inference and learning structure
    //val fg = createFG(model)
    //todo: Support for FG Real and Factorie Vector Nodes
    //todo: requires edges that can pass vectors as messages
    //todo:

    //GradientOptimizer.optimize(fg)









    println(items.value())


    items.seq.head.source.seq.head.current



  }
}


