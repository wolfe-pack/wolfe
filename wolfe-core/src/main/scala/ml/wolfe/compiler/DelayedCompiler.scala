package ml.wolfe.compiler

import com.typesafe.scalalogging.LazyLogging
import ml.wolfe.term._
import org.scalactic._

/**
 * This compiler delays compilation until it has received initial parameters and input values in order to
 * determine domain sizes etc.
 * @author riedel
 */
trait DelayedCompiler extends Compiler with LazyLogging  {

  case class CompilationError(term: Term[Any], msg: String) extends WolfeError

  def compile[T](term: Term[T]): Module[T] = {

    new Module[T] {

      var compiled: Module[T] = null
      var paramBindings: Bindings = _
      var inputBindings: Bindings = _
      var paramsNeedInit = false

      private def updateCompilation() {
        if (compiled == null) {
          logger.info(s"Compiling: $term.")
          compiled = compile(term, paramBindings, inputBindings) match {
            case Good(result) =>
              logger.info("Compilation completed")
              result
            case Bad(Every(errors)) =>
              sys.error("Compilation failed with errors: " + errors)
          }
        }
      }

      def gradient[G](param: Var[G]) = {
        compiled.gradient(param)
      }


      def param[P](param: Var[P]) = {
        compiled.param(param)
      }

      def init(bindings: Binding[Any]*) = {
        paramBindings = Bindings(bindings: _*)
        if (compiled != null) compiled.init(bindings: _*)
        else {
          paramsNeedInit = true
        }
      }

      def forward(bindings: Binding[Any]*) = {
        inputBindings = Bindings(bindings: _*)
        updateCompilation()
        if (paramsNeedInit) {
          compiled.init(paramBindings.toSeq: _*)
          paramsNeedInit = false
        }
        compiled.forward(bindings: _*)
      }

      def output() = {
        compiled.output()
      }

      def backward(output: T) = {
        compiled.backward(output)
      }

      def updateParameters(learningRate: Double) = {
        compiled.updateParameters(learningRate)
      }
    }

  }

  def compile[T](term: Term[T], paramBindings: Bindings, inputBindings: Bindings): Module[T] Or Every[WolfeError]

}
