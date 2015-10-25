package ml.wolfe.compiler

import breeze.linalg.DenseMatrix
import breeze.numerics.sigmoid
import ml.wolfe.Tensor
import ml.wolfe.term._

import scala.collection.mutable

/**
 * @author rockt
 */
object ND4SCompiler {


  //  def valueToArray(value:Any, dst:Array[Any]) = value match {
  //    case p:Product =>
  //  }

  def compile[T](term: Term[T]): Module[T] = {

    val var2InputBox = new mutable.HashMap[Var[Any], InputBox]()
    val var2ParamBox = new mutable.HashMap[Var[Any], ParamBox]()

    def compile(term: Term[Any], paramBindings: Bindings): Box = {
      val box = term match {
        case v: Var[_] if paramBindings.contains(v) =>
          var2ParamBox.getOrElseUpdate(v, {
            val result = new ParamBox(v)
            result
          })
        case v: Var[_] =>
          var2InputBox.getOrElseUpdate(v, {
            val result = new InputBox(v)
            result
          })
        case Sigmoid(arg) =>
          val argBox = compile(arg, paramBindings)
          new SigmoidBox(argBox)
        case TensorMul(arg1, arg2) =>
          val arg1Box = compile(arg1, paramBindings)
          val arg2Box = compile(arg2, paramBindings)
          new TensorProductBox(arg1Box, arg2Box)
        case ComponentPlus(arg1, arg2) =>
          val arg1Box = compile(arg1, paramBindings)
          val arg2Box = compile(arg2, paramBindings)
          new TensorPlusBox(arg1Box, arg2Box)
        case GetElement(arg, element) =>
          val argBox = compile(arg, paramBindings)
          new GetElementBox(argBox, element)
      }
      box
    }


    new Module[T] {

      var compiled: Box = null

      def gradient[G](param: Var[G]) = {
        val paramBox = var2ParamBox(param)
        paramBox.grad.asInstanceOf[G] //todo: this needs to be converted back
      }

      def init(bindings: Binding[Any]*) = {
        compiled = compile(term, Bindings(bindings: _*))
        for (binding <- bindings; box <- var2ParamBox.get(binding.variable)) {
          box.output = Table.toTable(binding.value)
        }
      }

      def forward(bindings: Binding[Any]*) = {
        for (binding <- bindings; box <- var2InputBox.get(binding.variable)) {
          box.output = Table.toTable(binding.value)
        }
        compiled.forward()
      }

      def output() = {
        compiled.output.tensor.asInstanceOf[T] //todo: this needs to convert back
      }

      def backward(output: T) = {
        compiled.backward(Table(output.asInstanceOf[Tensor]))
      }
    }

  }

}


class Table(numTables: Int = 0) {

  var tensor:Tensor = _
  val children = Array.ofDim[Table](numTables)

//  def apply(index: Int): Tensor = children(index).tensor
//
//  def update(index: Int, value: Tensor) = children(index).tensor = value

  def +=(that: Table, scale: Double = 1.0): Unit = {
    tensor += that.tensor * scale
    for (i <- children.indices) children(i) +=(that.children(i), scale)
  }

  //val isTensor:Array[Boolean]
  //updateGlobal(globalIndex:

}

object Table {
  def apply(tensors: Tensor*) = {
    val result = new Table(tensors.length)
    for (i <- tensors.indices) {
      result.children(i) = new Table()
      result.children(i).tensor = tensors(i)
    }
    result
  }
  
  def toTable(value: Any): Table = value match {
    case p: Product =>
      val values = p.productIterator.map(toTable).toIndexedSeq
      val table = new Table(values.length)
      for (i <- values.indices) table.children(i) = values(i)
      table
    case t:DenseMatrix[_] =>
      val result = new Table()
      result.tensor = t.asInstanceOf[Tensor]
      result
    case _ => sys.error(s"We can't convert $value to table")

  }

}

trait Module[T] {
  def gradient[G](param: Var[G]): G

  def backward(output: T)

  def output(): T

  def forward(bindings: Binding[Any]*)

  def init(bindings: Binding[Any]*)

}

trait Box {
  def forward()

  def backward(gradOutput: Table)

  def output: Table

  def gradInputs: Table

}

class ParamBox(val variable:Var[Any]) extends Box {
  var output: Table = _
  var grad: Table = _
  var gradInputs: Table = _

  def backward(gradOutput: Table) = {
    grad += gradOutput
  }

  def forward() = {

  }

  def update(learningRate: Double): Unit = {
    output +=(grad, learningRate)
  }

}

class InputBox(val variable:Var[Any]) extends Box {
  var output: Table = _
  var gradInputs: Table = _

  def forward() = {

  }

  def backward(gradOutput: Table) = {

  }

}

class SigmoidBox(input: Box) extends Box {
  val output = new Table(1)
  val gradInputs = new Table(1)

  def forward() = {
    input.forward()
    output.tensor = sigmoid(input.output.tensor)
  }

  def backward(gradOutput: Table) = {
    val y = sigmoid(input.output.tensor)
    val oneMinusY = y :* (-1) + 1.0
    gradInputs.tensor = (y :* oneMinusY) :* gradOutput.tensor
  }
}

class TensorProductBox(arg1: Box, arg2: Box) extends Box {
  val output = new Table(1)
  val gradInputs = new Table(2)

  def forward() = {
    arg1.forward()
    arg2.forward()
    output.tensor = arg1.output.tensor * arg2.output.tensor
  }

  def backward(gradOutput: Table) = {
    gradInputs.children(0).tensor = gradOutput.tensor * arg2.output.tensor.t
    gradInputs.children(1).tensor = arg1.output.tensor * gradOutput.tensor.t
  }
}

class TensorPlusBox(arg1: Box, arg2: Box) extends Box {
  val output = new Table(1)
  val gradInputs = new Table(2)

  def forward() = {
    arg1.forward()
    arg2.forward()
    output.tensor = arg1.output.tensor + arg2.output.tensor
  }

  def backward(gradOutput: Table) = {
    //todo
  }
}


class GetElementBox(arg:Box, index:Int) extends Box {
  var output:Table = _
  def forward() = {
    arg.forward()
    output = arg.output.children(index)
  }

  var gradInputs: Table = _

  def backward(gradOutput: Table) = {

  }
}