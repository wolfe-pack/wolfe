package ml.wolfe.term

/**
 * @author riedel
 */
object AllSettings {
  def apply[T](domains: IndexedSeq[Dom])(fun: Settings => T) = {
    def toVary = Settings.fromSeq(domains map (_.createSetting()))
    new AllSettings[T](domains, toVary)(fun)
  }
}

class AllSettings[T](val domains: IndexedSeq[Dom], toVaryCreator: => Settings)(fun: Settings => T) extends Iterable[T] {
  //val flattened = for (i <- 0 until atoms.length; j <- 0 until atoms(i).length) yield (atoms(i)(j), i)
  val atomToVar = (for ((d, i) <- domains.zipWithIndex) yield Array.fill[Int](d.lengths.discOff)(i)).flatten.toArray
  val offsets = domains.flatMap(d => Range(0, d.lengths.discOff)).toArray
  val dims = domains.toArray flatMap (d => d.dimensions.discDims)
  // .(for (d <- domains) yield d.dimensions.discDims).toArray
  val atomsLength = dims.length

  override val size = (dims map (_.size)).product

  def loopSettings(proc:Settings => Unit): Unit = {
    val it = iterator
    while (it.hasNext) {
      proc(it.toVary)
      it.next()
    }
  }

  trait IteratorOverSettings[T] extends Iterator[T] {
    def toVary:Settings
  }

  def iterator = new IteratorOverSettings[T] {

    val toVary = toVaryCreator

    reset()

    var index = atomsLength - 1
    var varIndex = atomToVar(index)
    var offset = offsets(index)

    def updateAtom(): Unit = {
      varIndex = atomToVar(index)
      offset = offsets(index)
    }

    def currentTarget = toVary(varIndex).disc(offset)

    def setTarget(value: Int) = toVary(varIndex).disc(offset) = value

    def incrementTarget() = toVary(varIndex).disc(offset) += 1

    def currentEnd = dims(index).end

    def hasNext = index >= 0


    def next() = {
      val result = fun(toVary)
      while (index >= 0 && (currentTarget == currentEnd - 1)) {
        setTarget(dims(index).start)
        index -= 1
        if (index >= 0) updateAtom()
      }
      //increase setting by one if we haven't yet terminated
      if (index >= 0) {
        incrementTarget()
        //depending on where we are in the array we bump up the settingId
        if (index < atomsLength - 1) {
          index = atomsLength - 1
          updateAtom()
        }
      }
      result
    }

    def reset(): Unit = {
      for (i <- 0 until atomsLength)
        toVary(atomToVar(i)).disc(offsets(i)) = dims(i).start
    }

  }


}
