package ml.wolfe.compiler.torch

import java.io.{File, PrintWriter}
import java.util.UUID

import breeze.linalg.DenseMatrix
import ml.wolfe.Language._
import ml.wolfe._
import ml.wolfe.compiler.{Module, DelayedCompiler}
import ml.wolfe.term.Var
import ml.wolfe.term._
import org.scalautils.Good

/**
 * @author riedel
 */
object TorchCompiler extends DelayedCompiler {

  object nn {
    case class Linear(params:Var[Tensor]) extends Term[Tensor => Tensor]
    case class Sequential(modules:List[Term[Tensor => Tensor]]) extends Term[Tensor => Tensor]
    case class Parallel(modules:List[Term[Tensor => Tensor]]) extends Term[Tensor => Tensor]
    case class Concat(modules:List[Term[Tensor => Tensor]]) extends Term[Tensor => Tensor]
  }

  def compile[T](term: Term[T], paramBindings: Bindings, inputBindings: Bindings) = {
    val client = new TorchZeroMQClient()
    val uuid = UUID.randomUUID().toString
    val scriptFile = File.createTempFile(uuid,".lua")
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
