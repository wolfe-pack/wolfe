package ml.wolfe.term

import scala.language.dynamics

/**
 * @author riedel
 */
trait Module[T] {
  def output: Data

  def forward(input: Data)

}

trait Data {
}

trait Stateful[T] {

}

trait Table extends Data {

}

trait Tensor extends Data {
  def storage: Storage
}

trait Storage {
}

//
//
//trait Statefull[T] {
//  def state: T
//
//  def :=(value: T)
//}
//
//class Store[T] extends Statefull[T] {
//  var state: T = _
//
//  def :=(value: T) = state = value
//}
//
//trait ModuleData
////case class Table(product:Product) extends ModuleData
////case class Tensor

object ModuleCompiler {
  //  def compile[T](term: STerm[T], parameters: Seq[Var[Any]]): Module[T] Or Every[ErrorMsg] = {
  //    term match {
  //      case v: Var[_] => if (parameters.contains(v)) {
  //        val module = new Module[T] {
  //          val output = new Store[T]
  //          val parameters = Seq.empty
  //
  //          def forward(input: Bindings) = {
  //            output := input(v)
  //            output.state
  //          }
  //        }
  //        Good(module)
  //      } else {
  //        val module = new Module[T] {
  //          val output = new Store[T]
  //          val parameters = Seq(v)
  //
  //          def forward(input: Bindings) = {
  //            output.state
  //          }
  //        }
  //        Good(module)
  //      }
  //
  //      case SeqApply(s, i) =>
  //        ???
  //
  //      case _ => Bad(One(TermNotSupported(term)))
  //
  //    }
  //    ???
  //  }
}
