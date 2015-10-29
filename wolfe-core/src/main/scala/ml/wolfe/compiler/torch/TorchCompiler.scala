package ml.wolfe.compiler.torch

import java.io.{File, PrintWriter}
import java.util.UUID

import breeze.linalg.DenseMatrix
import ml.wolfe.Language._
import ml.wolfe._
import ml.wolfe.compiler.DelayedCompiler
import ml.wolfe.term.{Bindings, Term}

/**
 * @author riedel
 */
object TorchCompiler extends DelayedCompiler {

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
    ???
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
