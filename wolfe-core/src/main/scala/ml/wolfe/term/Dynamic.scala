package ml.wolfe.term


trait Dynamic[T] {
  parent =>
  protected var children: List[Dynamic[_]] = Nil
  protected var owners: List[Dynamic[_]] = Nil

  def ownerCount = owners.size

  def childCount = children.size

  def descendentCount:Int = 1 + children.map(_.descendentCount).sum

  def printTree(indent:Int = 0):Unit = {
    for (_ <- 0 until indent) print(" ")
    try { print(value()) } catch { case _:Exception => println("Exception")}
    println(owners.map(_.value()).mkString(" (",",",")"))
    for (c <- children) c.printTree(indent + 2)
  }

  def size = 1

  protected def detach(): Unit = {
    for (owner <- owners) {
      owner.children = owner.children.filterNot(_ == parent)
    }
    owners = Nil
  }

  private def attach(child: Dynamic[_]): Unit = {
    children ::= child
    child.owners ::= parent
  }

  protected def update()

  def updateValue(): Unit = {
    update()
    children.reverse foreach (_.updateValue())
  }

  def value(): T

  def map[A](f: T => A):Dynamic[A] = new Dynamic[A] {
    parent.attach(this)

    private var current: A = _
    private var needsUpdate = true

    protected def update() = {
      if (owners.nonEmpty) needsUpdate = true
    }

    def value() = {
      if (needsUpdate) {
        current = f(parent.value())
        needsUpdate = false
      }
      current
    }
  }

  def flatMap[A](f: T => Dynamic[A]):Dynamic[A] = new Dynamic[A] {
    parent.attach(this)

    private var current: Dynamic[A] = _
    private var needsUpdate = true

    protected def update() = {
      if (current != null) current.detach()
      needsUpdate = true
    }


    override protected def detach() = {
      super.detach()
      if (current != null) current.detach()
    }

    def value() = {
      if (needsUpdate) {
        current = f(parent.value())
        current.updateValue()
        needsUpdate = false
      }
      current.value()
    }

  }

}

object Dynamic {

  def singleton[T](value:T) = sequential(IndexedSeq(value))

  def sequential[T](seq: IndexedSeq[T]): Dynamic[T] = new Dynamic[T] {
    private var _current = -1

    protected def update() = {
      _current = (_current + 1) % seq.length
    }

    def value() = seq(_current)

    override def size = seq.size
  }

  def shuffled[T](seq: IndexedSeq[T]): Dynamic[T] = new Dynamic[T] {
    import ml.wolfe.util.Math.random
    private var _seq = random.shuffle(seq)
    private var _current = -1

    protected def update() = {
      if (_current == _seq.size - 1) {
        _seq = random.shuffle(seq)
        _current = -1
      }
      _current = _current + 1
    }

    def value() = seq(_current)

    override def size = seq.size
  }

  object unapply3 {
    def unapply[T1, T2, T3](d: Dynamic[(T1, T2, T3)]) = {
      val t1 = d.map(_._1)
      val t2 = d.map(_._2)
      val t3 = d.map(_._3)
      Some((t1,t2,t3))
    }
  }


}


trait DynamicTerm[D <: DoubleDom, T] extends ProxyTerm[D] {
  def generator: Dynamic[T]

  override def evaluator() = new Evaluator {
    val eval = self.evaluator()

    def eval(inputs: Array[Setting], output: Setting) = {
      //generator.generateNext()
      generator.updateValue()
      eval.eval(inputs, output)
    }
  }

  override def differentiator(wrt: Seq[Var[Dom]]) = new Differentiator {
    val diff = self.differentiator(wrt)

    def forwardProp(current: Array[Setting]) = {
      //generator.generateNext()
      generator.updateValue()
//      generator.printTree()
//      println(generator.descendentCount)
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


