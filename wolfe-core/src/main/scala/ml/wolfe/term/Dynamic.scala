package ml.wolfe.term

/**
 * @author riedel
 */

trait Dynamic[+T] {

  self =>
  def value(): T

  def generators: List[DynamicGenerator[_]]

  def map[A](f: T => A): Dynamic[A] = new Dynamic[A] {
    def generators = self.generators

    private var _currentA: A = _
    generators.foreach(_.addListener { () =>
      _currentA = f(self.value())
    })

    def value() = {
      _currentA
    }

  }
  override def toString = s"Dynamic(${value()})"
}

trait Dynamic2[T] {
  parent =>
  private var children:List[Dynamic2[_]] = Nil

  protected def update()
  def updateValue(): Unit = {
    update()
    children foreach (_.updateValue())
  }
  def value():T

  def map[A](f:T => A) = new Dynamic2[A] {
    parent.children ::= this

    private var current:A =_
    protected def update() = {
      current = f(parent.value())
    }

    def value() = current
  }

  def flatMap[A](f:T=>Dynamic2[A]) = new Dynamic2[A] {
    parent.children ::= this

    private var current:Dynamic2[A] = _

    protected def update() = {
      current = f(parent.value())
      current.updateValue()
    }

    def value() = current.value()

  }

}

object Dynamic2 {
  def sequential[T](seq:IndexedSeq[T]):Dynamic2[T] = new Dynamic2[T] {
    private var _current = -1

    protected def update() = {
      _current = (_current + 1) % seq.length
    }
    def value() = seq(_current)
  }
}

trait DynamicTerm[D <: DoubleDom,T] extends ProxyTerm[D] {
  def generator:DynamicGenerator[T]
  override def evaluator() = new Evaluator {
    val eval = self.evaluator()
    def eval(inputs: Array[Setting], output: Setting) = {
      generator.generateNext()
      eval.eval(inputs, output)
    }
  }
  override def differentiator(wrt: Seq[Var[Dom]]) = new Differentiator {
    val diff = self.differentiator(wrt)
    def forwardProp(current: Array[Setting]) = {
      generator.generateNext()
      diff.forwardProp(current)
      activation := diff.activation
    }
    def term = diff.term
    def withRespectTo = diff.withRespectTo
    def backProp(error: Setting, gradient: Array[Setting]) = {
      diff.backProp(error, gradient)
    }
  }
}

trait DynamicGenerator[+T] {
  type Listener = () => Unit
  private var listeners: List[Listener] = Nil

  def updateValue()

  def generateNext(): Unit = {
    updateValue()
    //println("Updating listeners: " + listeners)
    for (l <- listeners) l()
  }

  def value: Dynamic[T]

  def addListener(listener: Listener): Unit = {
    //println("Adding listener " + listener)
    listeners ::= listener
  }

  def termsPerEpoch: Int = 1
}

trait Generator[+T] {
  def generateNext()

  def current(): T

  def termsPerEpoch: Int
}
