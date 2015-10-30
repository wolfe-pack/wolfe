package ml.wolfe.compiler.torch

import java.io.{File, PrintWriter}
import java.util.UUID

import breeze.linalg.DenseMatrix
import ml.wolfe.Language._
import ml.wolfe._
import ml.wolfe.compiler.{DelayedCompiler, Module}
import ml.wolfe.term.{Var, _}
import org.scalautils.{Every, Good, Or}


/**
 * @author riedel
 */
object TorchCompiler extends DelayedCompiler {

  sealed trait TorchTerm[+T] extends Term[T]

  object nn {

    case class Linear(params: Var[Tensor], arg: Term[Tensor], in: Int, out: Int) extends TorchTerm[Tensor]

  }

  def preCompile[T](term: Term[T], domains: Domains): Term[T] Or Every[CompilationError] = {

    def c[A](term: Term[A]) = preCompile(term, domains)

    term match {
      case TensorMul(v: Var[_], arg) =>
        domains(v) match {
          case TensorDom(List(d1, d2)) => for (t <- c(arg)) yield nn.Linear(v, t, d1, d2)
          case TensorDom(List(d1)) => for (t <- c(arg)) yield nn.Linear(v, t, d1, 1)
          case _ => Good(term)
        }
      case _=> Good(term)
    }
  }


  def compile[T](term: Term[T], paramBindings: Bindings, inputBindings: Bindings) = {
    val variableDomainBindings = (paramBindings ++ inputBindings) map {b => b.variable in Typer.deriveDomainFromValue(b.value)}
    val variableDomains = Domains(variableDomainBindings.toSeq:_*)
    val domains = Typer.domains(variableDomains)(term)


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
