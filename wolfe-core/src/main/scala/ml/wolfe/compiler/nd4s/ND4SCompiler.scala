package ml.wolfe.compiler.nd4s

import breeze.linalg.DenseMatrix
import breeze.numerics.sigmoid
import com.typesafe.scalalogging.slf4j.LazyLogging
import ml.wolfe.Tensor
import ml.wolfe.compiler.Module
import ml.wolfe.term._
import org.scalautils._

import scala.collection.mutable

/**
 * @author rockt
 */
object ND4SCompiler extends LazyLogging {

  case class CompilationError(term: Term[Any], msg: String)

  def deriveDomainFromValue(value: Any): Dom[Any] = value match {
    case d: DenseMatrix[_] => TensorDom(List(d.rows, d.cols))
    case p: Product =>
      val argDoms = p.productIterator.toList.map(deriveDomainFromValue)
      val constructor = p.getClass.getConstructors.head
      def construct(args:Seq[Any]) =
        constructor.newInstance(args.asInstanceOf[Seq[AnyRef]]:_*).asInstanceOf[Product]
      ProductDom(argDoms, construct)
  }

  def compile[T](term: Term[T]): Module[T] = {

    logger.info(s"Remembering for compilation: $term.")

    new Module[T] {

      val var2InputBox = new mutable.HashMap[Var[Any], InputBox]()
      val var2ParamBox = new mutable.HashMap[Var[Any], ParamBox]()
      var compiled: Box = null
      var paramBindings: Bindings = _
      var inputBindings: Bindings = _
      var paramBoxesNeedUpdate = false

      implicit def asGood(box: Box): Box Or Every[CompilationError] = Good(box)

      implicit def asBad(error: CompilationError): Bad[Nothing, One[CompilationError]] = Bad(One(error))


      def compile(termToCompileToBox: Term[Any]): Box Or Every[CompilationError] = {

        def tensorDom(dom: Dom[Any]): TensorDom Or One[CompilationError] = dom match {
          case t: TensorDom => Good(t)
          case _ => Bad(One(CompilationError(termToCompileToBox, s"Expected a tensor domain but got: $dom")))
        }

        def productDom(dom: Dom[Any]): ProductDom[Product] Or One[CompilationError] = dom match {
          case t: ProductDom[_] => Good(t)
          case _ => Bad(One(CompilationError(termToCompileToBox, s"Expected a product domain but got: $dom")))
        }

        def check[A](predicate: Boolean, result: A, msg: String) =
          if (predicate) Good(result) else Bad(One(CompilationError(termToCompileToBox, msg)))

        termToCompileToBox match {
          case v: Var[_] if paramBindings.contains(v) =>
            var2ParamBox.getOrElseUpdate(v, new ParamBox(v, deriveDomainFromValue(paramBindings(v))))

          case v: Var[_] if inputBindings.contains(v) =>
            var2InputBox.getOrElseUpdate(v, new InputBox(v, deriveDomainFromValue(inputBindings(v))))

          case v: Var[_] =>
            Bad(One(CompilationError(v, s"Variable $v has not been bound yet. Bind it using init() or forward().")))

          case Sigmoid(arg) =>
            for (argBox <- compile(arg); d <- tensorDom(argBox.dom))
              yield new SigmoidBox(argBox, d)

          case TensorMul(arg1, arg2) =>
            for (arg1Box <- compile(arg1); d1 <- tensorDom(arg1Box.dom);
                 arg2Box <- compile(arg2); d2 <- tensorDom(arg2Box.dom);
                 d <- check(d1.dims(1) == d2.dims(0), TensorDom(List(d1.dims(0), d2.dims(1))),
                   s"Tensor multiplication dimension mismatch: $d1 * $d2"))
              yield new TensorProductBox(arg1Box, arg2Box, d)

          case ComponentPlus(arg1, arg2) =>
            for (arg1Box <- compile(arg1); d1 <- tensorDom(arg1Box.dom);
                 arg2Box <- compile(arg2); d2 <- tensorDom(arg2Box.dom);
                 d <- check(d1.dims == d2.dims, d1, s"Tensor plus domains don't match: $d1 != $d2"))
              yield new TensorPlusBox(arg1Box, arg2Box, d)

          case GetElement(arg, element) =>
            for (argBox <- compile(arg); d <- productDom(argBox.dom))
              yield new GetElementBox(argBox, element, d.doms(element))
        }
      }

      private def updateCompilation() {
        if (compiled == null) {
          logger.info(s"Compiling: $term.")
          compiled = compile(term) match {
            case Good(box) =>
              logger.info("Compilation completed")
              box
            case Bad(Every(errors)) =>
              sys.error("Compilation failed with errors: " + errors)
          }
        }
      }

      def gradient[G](param: Var[G]) = {
        val paramBox = var2ParamBox(param)
        paramBox.grad.asInstanceOf[G] //todo: this needs to be converted back
      }

      def init(bindings: Binding[Any]*) = {
        paramBindings = Bindings(bindings: _*)
        paramBoxesNeedUpdate = true
      }

      def forward(bindings: Binding[Any]*) = {
        inputBindings = Bindings(bindings: _*)
        updateCompilation()
        for (binding <- inputBindings; box <- var2InputBox.get(binding.variable)) {
          box.output = Table.toTable(binding.value)
        }
        if (paramBoxesNeedUpdate) for (binding <- paramBindings; box <- var2ParamBox.get(binding.variable)) {
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

  var tensor: Tensor = _
  val children = Array.ofDim[Table](numTables)

  def +=(that: Table, scale: Double = 1.0): Unit = {
    tensor += that.tensor * scale
    for (i <- children.indices) children(i) +=(that.children(i), scale)
  }

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
    case t: DenseMatrix[_] =>
      val result = new Table()
      result.tensor = t.asInstanceOf[Tensor]
      result
    case _ => sys.error(s"We can't convert $value to table")

  }

}



trait Box {
  def forward()

  def backward(gradOutput: Table)

  def output: Table

  def gradInputs: Table

  def dom: Dom[Any]

}

class ParamBox(val variable: Var[Any], val dom: Dom[Any]) extends Box {
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

class InputBox(val variable: Var[Any], val dom: Dom[Any]) extends Box {
  var output: Table = _
  var gradInputs: Table = _

  def forward() = {

  }

  def backward(gradOutput: Table) = {

  }

}

class SigmoidBox(input: Box, val dom: TensorDom) extends Box {
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

class TensorProductBox(arg1: Box, arg2: Box, val dom: TensorDom) extends Box {
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

class TensorPlusBox(arg1: Box, arg2: Box, val dom: TensorDom) extends Box {
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


class GetElementBox(arg: Box, index: Int, val dom: Dom[Any]) extends Box {
  var output: Table = _

  def forward() = {
    arg.forward()
    output = arg.output.children(index)
  }

  var gradInputs: Table = _

  def backward(gradOutput: Table) = {

  }
}