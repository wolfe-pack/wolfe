package ml.wolfe.term

import java.util

import cc.factorie.la._
import ml.wolfe.{Mat, MoreArrayOps, Vect}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag


/**
 * A setting of a clique of discrete, continuous and vector variables.
 * @param numDisc number of discrete assignments.
 * @param numCont number of continuous assignments.
 * @param numVect number of vector assignments.
 */
final class Setting(numDisc: Int = 0, numCont: Int = 0, numVect: Int = 0, numMats: Int = 0, numSettings: Int = 0) {

  setting =>

  val disc = new DiscBuffer(numDisc)
  val cont = new ContBuffer(numCont)
  val vect = new VectBuffer(numVect)
  val mats = new MatrixBuffer(numMats)

  def resetToZero(): Unit = {
    disc.resetToZero()
    cont.resetToZero()
    vect.resetToZero()
    mats.resetToZero()
  }

  def randomize(eps: => Double): setting.type = {
    cont.randomize(eps)
    vect.randomize(eps)
    mats.randomize(eps)
    setting
  }

  final class DiscBuffer(val length: Int) extends Buffer[Int](setting) {
    def resetToZero(offset: Int) = {
      this(offset) = 0
      broadcastReset(offset)
    }

    def randomize(eps: => Double) = {}

    def add(index: Int, value: Int) = {

    }
  }


  final class ContBuffer(val length: Int) extends Buffer[Double](setting) {
    def *=(scale: Double): Unit = {
      for (i <- 0 until length) array(i) *= scale
      broadcastAllChanged()
    }

    def :=(scale: Double): Unit = {
      for (i <- 0 until length) array(i) = scale
      broadcastAllChanged()
    }

    def +=(that: Buffer[Double]): Unit = {
      for (i <- 0 until length) array(i) += that(i)
      broadcastAllChanged()
    }


    def add(index: Int, value: Double) = {
      array(index) += value
      broadcastChange(index)
    }

    def resetToZero(offset: Int) = {
      array(offset) = 0.0
      broadcastReset(offset)
    }

    def randomize(eps: => Double) = {
      for (i <- 0 until length) array(i) = array(i) + eps
      broadcastAllChanged()
    }
  }

  final class VectBuffer(val length: Int) extends Buffer[Vect](setting) {
    def *=(scale: Double): Unit = {
      for (i <- 0 until length) array(i) *= scale
      broadcastAllChanged()
    }

    def :=(scale: Double): Unit = {
      broadcastAllChanged()
      for (i <- 0 until length) array(i) := scale
    }

    def +=(that: Buffer[Vect]): Unit = {
      broadcastAllChanged()
      for (i <- 0 until length) add(i, that(i))
    }

    def randomize(eps: => Double) = {
      for (i <- 0 until length)
        for (j <- 0 until array(i).length)
          array(i)(j) += eps
      broadcastAllChanged()
    }

    def resetToZero(offset: Int) = {
      array(offset).zero()
      broadcastReset(offset)
    }

    def copyVector(v: Vect) = v match {
      case s: SingletonTensor1 => new SingletonTensor1(s.dim1, s.singleIndex, s.singleValue)
      case s: MutableSingletonTensor1 => new MutableSingletonTensor1(s.dim1, s.singleIndex, s.singleValue)
      case s: GrowableSparseHashTensor1 =>
        val result = new GrowableSparseHashTensor1(s.sizeProxy)
        result += s
        result
      case null => null
      case _ => v.copy
    }

    override def update(index: Int, value: Vect): Unit = {
      if (this(index) == null) {
        super.update(index, copyVector(value))
      } else {
        if (_adaptiveVectors) {
          (this(index), value) match {
            case (_: DenseTensor1, target: SparseIndexedTensor) =>
              super.update(index, copyVector(target))
            case (_: SparseIndexedTensor, target: DenseTensor1) =>
              super.update(index, copyVector(target))
            case (_, _) =>
              this(index) := value
              broadcastChange(index)
          }
        } else {
          this(index) := value
          broadcastChange(index)
        }
      }
    }


    def set(index: Int, value: Vect, scale: Double): Unit = {
      update(index, value)
      this(index) *= scale
    }


    def add(index: Int, value: Vect): Unit = {
      if (_adaptiveVectors) {
        if (this(index) == null) {
          this(index) = value.copy
        } else {
          (this(index), value) match {
            case (_, null) =>
            case (current: SparseIndexedTensor, arg: DenseTensor1) =>
              this(index) = arg.copy
              this(index) += current
            case (current: DenseTensor1, arg: SparseIndexedTensor) =>
              //this(index) = arg.copy
              this(index) += arg
            case (_, _) =>
              this(index) += value
              broadcastChange(index)
          }
        }
      } else {
        this(index) += value
      }
    }


  }

  final class MatrixBuffer(val length: Int) extends Buffer[Mat](setting) {
    def *=(scale: Double): Unit = {
      for (i <- 0 until length) array(i) *= scale
      broadcastAllChanged()
    }

    def :=(scale: Double): Unit = {
      for (i <- 0 until length) array(i) := scale
      broadcastAllChanged()
    }

    def +=(that: Buffer[Mat]): Unit = {
      for (i <- 0 until length) array(i) += that(i)
      broadcastAllChanged()
    }


    def add(index: Int, value: Mat) = {
      if (this(index) == null) {
        this(index) = value.copy
      } else {
        (this(index), value) match {
          case (_, null) =>
          case (_, _) =>
            array(index) += value
            broadcastChange(index)
        }
      }
    }

    override def update(index: Int, value: Mat): Unit = {
      if (this(index) == null) {
        super.update(index, value.copy)
      } else {
        this.array(index) = value
        broadcastChange(index)
      }

    }


    def resetToZero(offset: Int) = {
      array(offset) := 0
      broadcastReset(offset)
    }

    def randomize(eps: => Double) = {
      for (i <- 0 until length)
        for (j <- 0 until array(i).length)
          array(i)(j) += eps
      broadcastAllChanged()
    }

  }

  private var _adaptiveVectors = false
  var informListeners = true

  def setAdaptiveVectors(adaptive: Boolean): Unit = {
    _adaptiveVectors = adaptive
  }

  def adaptiveVectors = _adaptiveVectors


  def shallowCopyTo(target: Setting, targetOffsets: Offsets, targetMultiplier: Int): Unit = {
    disc.shallowCopyTo(target.disc, 0, targetOffsets.discOff * targetMultiplier, disc.length)
    cont.shallowCopyTo(target.cont, 0, targetOffsets.contOff * targetMultiplier, cont.length)
    vect.shallowCopyTo(target.vect, 0, targetOffsets.vectOff * targetMultiplier, vect.length)
    mats.shallowCopyTo(target.mats, 0, targetOffsets.matsOff * targetMultiplier, mats.length)
  }

  def shallowCopyTo(target: Setting, srcOffsets: Offsets, targetOffsets: Offsets, length: Offsets): Unit = {
    disc.shallowCopyTo(target.disc, srcOffsets.discOff, targetOffsets.discOff, length.discOff)
    cont.shallowCopyTo(target.cont, srcOffsets.contOff, targetOffsets.contOff, length.contOff)
    vect.shallowCopyTo(target.vect, srcOffsets.vectOff, targetOffsets.vectOff, length.vectOff)
    mats.shallowCopyTo(target.mats, srcOffsets.matsOff, targetOffsets.matsOff, length.matsOff)
  }

  def deepCopyTo(target: Setting, srcOffsets: Offsets, targetOffsets: Offsets, length: Offsets): Unit = {
    disc.deepCopyTo(target.disc, srcOffsets.discOff, targetOffsets.discOff, length.discOff)
    cont.deepCopyTo(target.cont, srcOffsets.contOff, targetOffsets.contOff, length.contOff)
    vect.deepCopyTo(target.vect, srcOffsets.vectOff, targetOffsets.vectOff, length.vectOff)
    mats.deepCopyTo(target.mats, srcOffsets.matsOff, targetOffsets.matsOff, length.matsOff)
  }

  def shallowCopyTo(target: Setting, srcElementLength: Offsets, srcMultiplier: Int, tgtElementLength: Offsets, tgtMultiplier: Int,
                    length: Offsets, srcOffsets: Offsets = Offsets(), tgtOffsets: Offsets = Offsets()): Unit = {
    disc.shallowCopyTo(target.disc, srcOffsets.discOff + length.discOff * srcMultiplier,
      tgtOffsets.discOff + tgtElementLength.discOff * tgtMultiplier, length.discOff)
    cont.shallowCopyTo(target.cont, srcOffsets.contOff + length.contOff * srcMultiplier,
      tgtOffsets.contOff + tgtElementLength.contOff * tgtMultiplier, length.contOff)
    vect.shallowCopyTo(target.vect, srcOffsets.vectOff + length.vectOff * srcMultiplier,
      tgtOffsets.vectOff + tgtElementLength.vectOff * tgtMultiplier, length.vectOff)
    mats.shallowCopyTo(target.mats, srcOffsets.matsOff + length.matsOff * srcMultiplier,
      tgtOffsets.matsOff + tgtElementLength.matsOff * tgtMultiplier, length.matsOff)
  }


  def *=(scale: Double): Unit = {
    cont *= scale
    vect *= scale
    mats *= scale
  }

  def +=(that: Setting): Unit = {
    disc shallowAssign that.disc //todo: this is odd but required to pass on discrete values in gradients
    cont += that.cont
    vect += that.vect
    mats += that.mats

  }

  def :=(value: Double): Unit = {
    cont := value
    vect := value
    mats := value
  }

  def deepAssign(that: Setting): Unit = {
    disc deepAssign that.disc
    cont deepAssign that.cont
    vect deepAssign that.vect
    mats deepAssign that.mats
  }


  def shallowAssign(that: Setting): Unit = {
    disc shallowAssign that.disc
    cont shallowAssign that.cont
    vect shallowAssign that.vect
    mats shallowAssign that.mats
  }

  def shallowCopy(to: Setting) = {
    disc.shallowCopyTo(to.disc, 0, 0, disc.length)
    cont.shallowCopyTo(to.cont, 0, 0, cont.length)
    vect.shallowCopyTo(to.vect, 0, 0, vect.length)
    mats.shallowCopyTo(to.mats, 0, 0, mats.length)
  }

  def shallowAssign(src: Setting, srcOffsets: Offsets, lengths: Offsets, tgtOffsets: Offsets = Offsets.zero): Unit = {
    disc shallowAssign(src.disc, srcOffsets.discOff, lengths.discOff, tgtOffsets.discOff)
    cont shallowAssign(src.cont, srcOffsets.contOff, lengths.contOff, tgtOffsets.contOff)
    vect shallowAssign(src.vect, srcOffsets.vectOff, lengths.vectOff, tgtOffsets.vectOff)
    mats shallowAssign(src.mats, srcOffsets.matsOff, lengths.matsOff, tgtOffsets.matsOff)
  }

  def deepAssign(src: Setting, srcOffsets: Offsets, lengths: Offsets, tgtOffsets: Offsets = Offsets.zero): Unit = {
    disc deepAssign(src.disc, srcOffsets.discOff, lengths.discOff, tgtOffsets.discOff)
    cont deepAssign(src.cont, srcOffsets.contOff, lengths.contOff, tgtOffsets.contOff)
    vect deepAssign(src.vect, srcOffsets.vectOff, lengths.vectOff, tgtOffsets.vectOff)
    mats deepAssign(src.mats, srcOffsets.matsOff, lengths.matsOff, tgtOffsets.matsOff)
  }

  def ensureSparsity(): Unit = {
    for (i <- 0 until vect.length) {
      vect(i) match {
        case d: DenseTensor1 =>
          vect(i) = new SparseTensor1(d.dim1)
        case _ =>
      }
    }

  }


  def epsEquals(eps: Double, that: Setting): Boolean = {
    if (disc.length != that.disc.length) return false
    if (cont.length != that.cont.length) return false
    if (vect.length != that.vect.length) return false
    if (mats.length != that.mats.length) return false

    for (i <- 0 until disc.length) if (disc(i) != that.disc(i)) return false
    for (i <- 0 until cont.length) if (math.abs(cont(i) - that.cont(i)) > eps) return false
    for (i <- 0 until vect.length; j <- 0 until vect(i).size) if (math.abs(vect(i)(j) - that.vect(i)(j)) > eps) return false
    for (i <- 0 until mats.length; j <- 0 until mats(i).size) if (math.abs(mats(i)(j) - that.mats(i)(j)) > eps) return false

    true
  }

  override def toString = {
    def renderIfNotEmpty(buffer: Buffer[_]) = {
      if (buffer.length > 0) Some(buffer.mkString(" ")) else None
    }
    val rendered = Seq(renderIfNotEmpty(disc), renderIfNotEmpty(cont), renderIfNotEmpty(vect), renderIfNotEmpty(mats))
    rendered.flatten.mkString("\n")
  }
}

object Setting {

  def cont(value: Double) = {
    val result = new Setting(numCont = 1)
    result.cont(0) = value
    result
  }

  def disc(value: Int) = {
    val result = new Setting(numDisc = 1)
    result.disc(0) = value
    result
  }


  def merge(settings: Array[Setting], result: Setting) = {
    var cont = 0
    var disc = 0
    var vect = 0
    var mats = 0
    for (setting <- settings) {
      for (i <- 0 until setting.disc.length) {
        result.disc(disc) = setting.disc(i)
        disc += 1
      }
      for (i <- 0 until setting.cont.length) {
        result.cont(cont) = setting.cont(i)
        cont += 1
      }
      for (i <- 0 until setting.vect.length) {
        result.vect(vect) = setting.vect(i)
        vect += 1
      }
      for (i <- 0 until setting.mats.length) {
        result.mats(mats) = setting.mats(i)
        mats += 1
      }
    }
  }
}

final class Settings(val length: Int) extends IndexedSeq[Setting] {
  val array = Array.ofDim[Setting](length)

  def apply(index: Int) = array(index)

  def update(index: Int, value: Setting): Unit = {
    array(index) = value
  }

  def toValues(doms: Seq[Dom]): Seq[Any] = for ((d, s) <- doms zip this) yield d.toValue(s)

  //def toValues(vars:Seq[Variable]):Seq[Any] = toValues(vars map (_.domain))


  def linkedSettings(from: Seq[Var[Dom]], to: Seq[Var[Dom]]): Settings = {
    val mapping = VariableMapping(from, to)
    val result = new Settings(to.length)
    mapping.linkTargetsToSource(this, result)
    result
  }

  def :=(that: Settings): Unit = {
    for (i <- 0 until length) {
      this(i) shallowAssign that(i)
    }
  }

}

object Settings {
  def fromSeq(seq: Seq[Setting]): Settings = {
    val indexed = seq.toIndexedSeq
    val result = new Settings(seq.length)
    for (i <- 0 until result.length) result(i) = indexed(i)
    result
  }

  def apply(settings: Setting*) = fromSeq(settings.toIndexedSeq)
}

class DiscMsg(var msg: Array[Double]) {
  def this(size: Int) = this(Array.ofDim[Double](size))

  def argmax() = Range(0, msg.length).maxBy(msg)

  def isDiracAt(index: Int) =
    Range(0, msg.length).filterNot(_ == index).forall(msg(_) == Double.NegativeInfinity)
}

class ContMsg(var mean: Double = 0.0)

class VectMsg(var mean: Vect = null)

class MatsMsg(var mean: Mat = null)


class Msg(numDisc: Int = 0, numCont: Int = 0, numVect: Int = 0, numMats: Int = 0) {

  import MoreArrayOps._

  final var disc = Array.ofDim[DiscMsg](numDisc)
  final var cont = Array.ofDim[ContMsg](numCont)
  final var vect = Array.ofDim[VectMsg](numVect)
  final var mats = Array.ofDim[MatsMsg](numMats)

  final def :=(msg: Msg): Unit = {
    assert(disc.length == msg.disc.length && cont.length == msg.cont.length &&
      vect.length == msg.vect.length && mats.length == msg.mats.length)
    disc = msg.disc.clone()
    cont = msg.cont.clone()
    vect = msg.vect.clone()
    mats = msg.mats.clone()
  }

  override final def clone(): Msg = {
    val res = new Msg(numDisc, numCont, numVect, numMats)
    res := this
    res
  }

  final def +(value: Msg): Msg = {
    val res = this.clone()
    res += value
    res
  }

  final def -(value: Msg): Msg = {
    val res = this.clone()
    res -= value
    res
  }

  final def :=(value: Double): Unit = {
    for (i <- 0 until disc.length) util.Arrays.fill(disc(i).msg, value)
    for (i <- 0 until cont.length) cont(i).mean = value
    for (i <- 0 until vect.length) if (vect(i) != null) vect(i).mean := value
    for (i <- 0 until mats.length) if (mats(i) != null) mats(i).mean := value
  }

  final def +=(value: Msg): Unit = {
    for (i <- 0 until disc.length) {
      incr(disc(i).msg, value.disc(i).msg)
    }
    for (i <- 0 until cont.length) {
      cont(i).mean += value.cont(i).mean
    }
  }

  final def -=(value: Msg): Unit = {
    for (i <- 0 until disc.length) {
      val d = disc(i).msg
      val v = value.disc(i).msg
      require(v.length == d.length)
      var idx = 0
      while (idx < d.length) {
        if (v(idx).isNegInfinity) d(idx) = 0.0
        else d(idx) -= v(idx)
        idx += 1
      }
    }
    for (i <- 0 until cont.length) {
      cont(i).mean -= value.cont(i).mean
    }
  }

  def argmax(target: Setting, offsets: Offsets = Offsets.zero): Unit = {

    for (i <- 0 until disc.length) {
      val argmaxIndex = maxIndex(disc(i).msg)
      target.disc(i + offsets.discOff) = argmaxIndex
    }
    for (i <- 0 until cont.length) {
      target.cont(i + offsets.contOff) = cont(i).mean
    }

  }

}

final class Msgs(val length: Int) extends IndexedSeq[Msg] {
  val array = Array.ofDim[Msg](length)

  def apply(idx: Int) = array(idx)

  def update(idx: Int, msg: Msg): Unit = {
    array(idx) = msg
  }
}

object Msgs {
  def apply(msg1:Msg, other: Msg*):Msgs = {
    val msgs = msg1 +: other
    apply(msgs)
  }

  def apply(msgs: Seq[Msg]):Msgs = {
    val result = new Msgs(msgs.length)
    for ((msg, i) <- msgs.zipWithIndex) result(i) = msg
    result
  }


}


final class VariableMapping(val srcIndex: Array[Int], val tgtIndex: Array[Int]) {

  def length = srcIndex.length

  lazy val pairs = srcIndex zip tgtIndex

  def copyForwardDeep(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) tgt(tgtIndex(i)) shallowAssign src(srcIndex(i))
  }

  def copyForwardDeep(src: Settings, tgt: Settings) = {
    for (i <- 0 until srcIndex.length)
      tgt(tgtIndex(i)) shallowAssign src(srcIndex(i))
  }


  def copyForwardShallow(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) tgt(tgtIndex(i)) = src(srcIndex(i))
  }

  def linkTargetsToSource(src: Settings, tgt: Settings) = {
    for (i <- 0 until srcIndex.length) tgt(tgtIndex(i)) = src(srcIndex(i))
  }


  def copyBackwardDeep(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) src(srcIndex(i)) shallowAssign tgt(tgtIndex(i))
  }

  def copyBackwardDeep(src: Settings, tgt: Settings) = {
    for (i <- 0 until srcIndex.length) src(srcIndex(i)) shallowAssign tgt(tgtIndex(i))
  }

  def addBackward(src: Settings, tgt: Settings) = {
    for (i <- 0 until srcIndex.length) src(srcIndex(i)) += tgt(tgtIndex(i))
  }


  def copyBackwardShallow(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) src(srcIndex(i)) = tgt(tgtIndex(i))
  }

  def getTgtIndex(src: Int): Int = {
    var i = 0
    while (i < srcIndex.length) {
      if (srcIndex(i) == src) return tgtIndex(i)
      i += 1
    }
    -1
  }

}


trait BufferListener {

  def reset(index: Int)

  def changed(index: Int)

  def changed(indices: Iterable[Int])

  def allChanged()
}

class BufferChangeRecorder[T](val buffer: Buffer[T], initAllChanges: Boolean = true) extends BufferListener {

  buffer.listeners += this

  private var changedIndices = new mutable.HashSet[Int]
  private var resetIndices = new mutable.HashSet[Int]


  if (initAllChanges) allChanged()

  def changes = changedIndices

  def resets = resetIndices

  def changed(index: Int) = {
    changedIndices += index
  }

  def changed(indices: Iterable[Int]) = {
    changedIndices ++= indices
  }


  def reset(index: Int) = {
    resetIndices += index
  }

  def allChanged() = {
    changedIndices ++= Range(0, buffer.length)
  }

  def forget(): Unit = {
    changedIndices = new mutable.HashSet[Int]
    resetIndices = new mutable.HashSet[Int]
    //changedIndices.clear() todo: this was slow because clearing the hashmap only meant setting entries to null.
    //resetIndices.clear()
  }

  def addIfChanged(tgt: Buffer[T]): Unit = {
    changedIndices foreach (i => tgt.add(i, buffer(i)))
  }


}

class SettingChangeRecorder(val setting: Setting, initAllChanges: Boolean = true) {

  def createRecorder[T](buffer: Buffer[T]) = {
    new BufferChangeRecorder[T](buffer, initAllChanges)
  }

  val disc = createRecorder(setting.disc)
  val cont = createRecorder(setting.cont)
  val vect = createRecorder(setting.vect)
  val mats = createRecorder(setting.mats)

  def setChangesToZero(): Unit = {
    disc.changes.toSet foreach setting.disc.resetToZero //using a toSet to make sure we work with a copy
    cont.changes.toSet foreach setting.cont.resetToZero
    vect.changes.toSet foreach setting.vect.resetToZero
    mats.changes.toSet foreach setting.mats.resetToZero
  }

  def forget(): Unit = {
    disc.forget()
    cont.forget()
    vect.forget()
    mats.forget()
  }

  def addIfChanged(tgt: Setting): Unit = {
    cont.addIfChanged(tgt.cont)
    vect.addIfChanged(tgt.vect)
    mats.addIfChanged(tgt.mats)
  }

  def shallowCopyToIfChanged(target: Setting, srcOffsets: Offsets, targetOffsets: Offsets, lengths: Offsets): Unit = {
    disc.buffer.shallowCopyTo(target.disc, srcOffsets.discOff, targetOffsets.discOff, lengths.discOff, disc.changes)
    cont.buffer.shallowCopyTo(target.cont, srcOffsets.contOff, targetOffsets.contOff, lengths.contOff, cont.changes)
    vect.buffer.shallowCopyTo(target.vect, srcOffsets.vectOff, targetOffsets.vectOff, lengths.vectOff, vect.changes)
    mats.buffer.shallowCopyTo(target.mats, srcOffsets.matsOff, targetOffsets.matsOff, lengths.matsOff, mats.changes)
  }

  def deepCopyToIfChanged(target: Setting, srcOffsets: Offsets, targetOffsets: Offsets, lengths: Offsets): Unit = {
    disc.buffer.deepCopyTo(target.disc, srcOffsets.discOff, targetOffsets.discOff, lengths.discOff, disc.changes)
    cont.buffer.deepCopyTo(target.cont, srcOffsets.contOff, targetOffsets.contOff, lengths.contOff, cont.changes)
    vect.buffer.deepCopyTo(target.vect, srcOffsets.vectOff, targetOffsets.vectOff, lengths.vectOff, vect.changes)
    mats.buffer.deepCopyTo(target.mats, srcOffsets.matsOff, targetOffsets.matsOff, lengths.matsOff, mats.changes)
  }


}

abstract class Buffer[T: ClassTag](val setting: Setting) {
  def length: Int

  val array = Array.ofDim[T](length)

  val listeners = new ArrayBuffer[BufferListener]()

  def add(index: Int, value: T): Unit

  def resetToZero(offset: Int): Unit

  def resetToZero(): Unit = {
    //todo this informs listeners on a per index basis instead of in a batch way
    for (i <- 0 until length) resetToZero(i)
  }

  def broadcastAllChanged() {
    if (shouldBroadcast) for (l <- listeners) l.allChanged()
  }

  def broadcastChange(offset: Int): Unit = {
    if (shouldBroadcast) listeners foreach (_.changed(offset))
  }

  def broadcastReset(offset: Int): Unit = {
    if (shouldBroadcast) listeners foreach (_.reset(offset))
  }


  def broadcastChanges(offsets: Iterable[Int]): Unit = {
    if (shouldBroadcast) listeners foreach (_.changed(offsets))
  }

  def shouldBroadcast = setting.informListeners

  def update(index: Int, value: T) = {
    array(index) = value
    broadcastChange(index)
  }

  def foreach(f: T => Unit) = (0 until length).foreach(x => f(apply(x)))

  def apply(index: Int) = array(index)

  def shallowCopyTo(tgt: Buffer[T], srcPos: Int, tgtPos: Int, length: Int): Unit = {
    System.arraycopy(array, srcPos, tgt.array, tgtPos, length)
    tgt.broadcastChanges(Range(tgtPos, tgtPos + length))
  }

  def deepCopyTo(tgt: Buffer[T], srcPos: Int, tgtPos: Int, length: Int): Unit = {
    for (i <- 0 until length) tgt(tgtPos + i) = this(srcPos + i)
  }

  def shallowCopyTo(tgt: Buffer[T], srcPos: Int, tgtPos: Int, length: Int, filter: collection.Set[Int]): Unit = {
    if (filter.nonEmpty) {
      val toCopy = filter filter (i => i >= srcPos && i < srcPos + length)
      toCopy foreach (i => tgt.array(i - srcPos + tgtPos) = array(i))
      tgt.broadcastChanges(toCopy map (i => i - srcPos + tgtPos))
    }
  }

  def deepCopyTo(tgt: Buffer[T], srcPos: Int, tgtPos: Int, length: Int, filter: collection.Set[Int]): Unit = {
    val toCopy = filter filter (i => i >= srcPos && i < srcPos + length)
    toCopy foreach (i => tgt(i - srcPos + tgtPos) = this(i))
  }


  def shallowAssign(value: Buffer[T]): Unit = {
    System.arraycopy(value.array, 0, array, 0, length)
    broadcastAllChanged()
  }

  def deepAssign(value: Buffer[T]): Unit = {
    for (i <- 0 until value.length) this(i) = value(i)
  }

  def shallowAssign(src: Buffer[T], srcOffset: Int, srcLength: Int, tgtOffset: Int = 0): Unit = {
    //todo: this can be implemented via shallowCopyTo, or vice versa
    System.arraycopy(src.array, srcOffset, array, tgtOffset, srcLength)
    broadcastAllChanged()
  }

  def deepAssign(src: Buffer[T], srcOffset: Int, srcLength: Int, tgtOffset: Int = 0): Unit = {
    //todo: this can be implemented via shallowCopyTo, or vice versa
    for (i <- 0 until srcLength) this(tgtOffset + i) = src(srcOffset + i)
  }


  def randomize(eps: => Double)

  def mkString(sep: String) = array.mkString(sep)
}


object VariableMapping {
  def apply(src: Seq[Var[Dom]], tgt: Seq[Var[Dom]]) = {
    val pairs = src.indices.view.map(i => i -> tgt.indexOf(src(i))).filter(_._2 != -1).toArray
    val (srcIndex, tgtIndex) = (pairs.map(_._1), pairs.map(_._2))
    new VariableMapping(srcIndex, tgtIndex)
  }
}

case class Dimensions(discDims: Array[Range] = Array.empty) {
  def +(that: Dimensions) = copy(discDims ++ that.discDims)

  def *(times: Int) = {
    val result = Array.ofDim[Range](discDims.length * times)
    for (i <- 0 until times)
      System.arraycopy(discDims, 0, result, i * discDims.length, discDims.length)
    Dimensions(result)
  }
}

case class Offsets(discOff: Int = 0, contOff: Int = 0, vectOff: Int = 0, matsOff: Int = 0, settingsOff: Int = 0) {
  def +(disc: Int, cont: Int, vect: Int, mats: Int, settings: Int) =
    Offsets(discOff + disc, contOff + cont, vectOff + vect, matsOff + mats, settingsOff + settings)

  def +(that: Offsets, scale: Int = 1) =
    Offsets(discOff + scale * that.discOff,
      contOff + scale * that.contOff,
      vectOff + scale * that.vectOff,
      matsOff + scale * that.matsOff,
      settingsOff + scale * that.settingsOff)

  def *(scale: Int) =
    Offsets(scale * discOff, scale * contOff, scale * vectOff, scale * matsOff, scale * settingsOff)
}

object Offsets {
  val zero = new Offsets()
}


//case class Ranges(from: Offsets, to: Offsets) {
//  def copy(src: Setting, tgt: Setting): Unit = {
//    src.disc.copyTo(tgt.disc, from.discOff, 0, to.discOff - from.discOff)
//    src.cont.copyTo(tgt.cont, from.contOff, 0, to.contOff - from.contOff)
//    src.vect.copyTo(tgt.vect, from.vectOff, 0, to.vectOff - from.vectOff)
//    src.mats.copyTo(tgt.mats, from.matsOff, 0, to.matsOff - from.matsOff)
//  }
//
//  def addInto(src: Setting, tgt: Setting): Unit = {
//    for (i <- 0 until numDisc) {
//      tgt.disc(from.contOff + i) = src.disc(i)
//    }
//    for (i <- 0 until numCont) {
//      tgt.cont(from.contOff + i) += src.cont(i)
//    }
//
//    for (i <- 0 until numVect) {
//      tgt.vect.add(from.vectOff + i, src.vect(i))
//    }
//
//    for (i <- 0 until numMats) {
//      tgt.mats(from.matsOff + i) += src.mats(i)
//    }
//  }
//
//  def addIntoIfChanged(src: Setting, tgt: Setting): Unit = {
//    for (i <- src.disc.changed()) {
//      tgt.disc(from.contOff + i) = src.disc(i)
//    }
//    for (i <- src.cont.changed()) {
//      tgt.cont(from.contOff + i) += src.cont(i)
//    }
//
//    for (i <- src.vect.changed()) {
//      tgt.vect.add(from.vectOff + i, src.vect(i))
//    }
//
//    for (i <- src.mats.changed()) {
//      tgt.mats(from.matsOff + i) += src.mats(i)
//    }
//  }
//
//
//  def numDisc = to.discOff - from.discOff
//
//  def numCont = to.contOff - from.contOff
//
//  def numVect = to.vectOff - from.vectOff
//
//  def numMats = to.matsOff - from.matsOff
//}



