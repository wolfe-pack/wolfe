package ml.wolfe.compiler

import ml.wolfe.term.{Bindings, Binding, Var}

/**
  * A module is a compiled term that supports operations related to the term, such as forward and backward
  * evaluation. Modules distinguish between two types of variables: inputs and parameters. They can be seen
  * as functions from assignments to the input variables to output values, controlled
  * by the module parameters. Note that a module is stateful in the sense that it stores a current assignment
  * to its parameters.
  * @author riedel
  */
trait Module[T] {
  /**
    * Before a module can be used its parameters need to be initialized. This method
    * sets the internal module parameters based on the given bindings. By calling this method
    * the user also implicitly determines which term variables are parameters: every variable not
    * given in the initial bindings is an input variable.
    * @param bindings a variable argument sequence of parameter bindings (of the form parameter := value).
    */
  def init(bindings: Binding[Any]*)

  /**
    * Applies forward evaluation to the given input based on the current parameters.
    * @param bindings bindings to the input variables. This needs to contain bindings for all variables
    *                 that have not been initialized as parameters.
    */
  def forward(bindings: Binding[Any]*)

  /**
    * Performs a backward pass starting with the given output, using the input binding from the last call to
    * [[ml.wolfe.compiler.Module#forward]].
    * @param output the output gradient to start backward evaluation with.
    */
  def backward(output: T)

  /**
    * Returns the current output of the module as calculated by [[ml.wolfe.compiler.Module#forward]].
    * @return the current output.
    */
  def output(): T

  /**
    * Returns the gradient of the given parameter based on the last call to [[ml.wolfe.compiler.Module#backward]]
    * @param param the parameter to get the gradient for.
    * @tparam G the value type of the parameter.
    * @return the current gradient for `param`.
    */
  def gradient[G](param: Var[G]): G

  /**
    * Return the current value of the given parameter.
    * @param param the parameter to get the value for.
    * @tparam P the value type of the parameter.
    * @return the current value of the parameter.
    */
  def param[P](param: Var[P]): P

  /**
    * Updates the parameters by adding learningRate * gradient to them.
    * @param learningRate the rate with which we update the parameters.
    */
  def updateParameters(learningRate:Double)


  def train(data: Seq[(Bindings, T)], params: TrainParams) = ???

}

case class TrainParams(iterations: Int, learningRate: Double)