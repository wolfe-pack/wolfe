package ml.wolfe.compiler.torch

import java.io.{File, PrintWriter}
import java.util.UUID

import breeze.linalg.DenseMatrix
import ml.wolfe.Language._
import ml.wolfe._
import ml.wolfe.compiler.{DelayedCompiler, Module}
import ml.wolfe.term.Typer.TypedTerm
import ml.wolfe.term.{Var, _}
import org.scalautils.Accumulation._
import org.scalautils.{Every, Good, Or}


/**
 * @author riedel
 */
object TorchCompiler extends DelayedCompiler {

  sealed trait TorchTerm[+T] extends Term[T]

  object nn {

    case class Linear(params: Var[Tensor], arg:Term[Tensor]) extends TorchTerm[Tensor]

  }


  def preCompile[T](term: Term[T], paramBindings: Bindings, inputBindings: Bindings): TypedTerm[T] Or Every[CompilationError] = {

    def c[A](term: Term[A]) = preCompile(term,paramBindings,inputBindings)

    term match {
      case TensorMul(v:Var[_],arg) =>
        c(arg) match {
          case Good(TypedTerm(input,TensorDom(dims))) => Good(TypedTerm(nn.Linear(v,input),???))
          case b => b
        }
    }
    ???
  }


  def compile[T](term: Term[T], paramBindings: Bindings, inputBindings: Bindings) = {
    val client = new TorchZeroMQClient()
    val uuid = UUID.randomUUID().toString
    val scriptFile = File.createTempFile(uuid, ".lua")
    val scriptOut = new PrintWriter(scriptFile)
    logger.info(s"Creating lua file: $scriptFile")
    val script =
      """
        |function test()
        |  return "Test"
        |end
      """.stripMargin
    scriptOut.println(script)
    scriptOut.close()
    client.call("dofile")(scriptFile.getAbsolutePath)
    val result = client.call("test")()
    println(result)
    Good(new Module[T] {
      def gradient[G](param: Var[G]) = ???

      def init(bindings: Binding[Any]*) = {
        //call module.params(W) = ???)
      }

      def forward(bindings: Binding[Any]*) = ???

      def output() = ???

      def backward(output: T) = ???
    })
  }

  def main(args: Array[String]) {
    val W = Var[Tensor]("W")
    val x = Var[Tensor]("x")
    val term = sigmoid(W * x)

    val module = TorchCompiler.compile(term)
    module.init(W := DenseMatrix.ones(2, 2))
    module.forward(x := DenseMatrix(1.0, 2.0))

  }
}
