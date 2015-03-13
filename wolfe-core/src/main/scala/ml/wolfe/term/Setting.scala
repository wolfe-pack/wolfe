package ml.wolfe.term

import java.lang.System._
import java.util

import cc.factorie.la.{SparseIndexedTensor1, DenseTensor1, SparseTensor1}
import ml.wolfe.{MoreArrayOps, FactorieMatrix, FactorieVector}

import scala.collection.mutable
import scala.reflect.ClassTag


/**
 * A setting of a clique of discrete, continuous and vector variables.
 * @param numDisc number of discrete assignments.
 * @param numCont number of continuous assignments.
 * @param numVect number of vector assignments.
 */
final class Setting(numDisc: Int = 0, numCont: Int = 0, numVect: Int = 0, numMats: Int = 0) {

  setting =>

  val disc = new DiscBuffer(numDisc)
  val cont = new ContBuffer(numCont)
  val vect = new VectBuffer(numVect)
  val mats = new MatrixBuffer(numMats)

  def clearChangeRecord(): Unit = {
    disc.resetChanges()
    cont.resetChanges()
    vect.resetChanges()
    mats.resetChanges()
  }

  def resetToZero(): Unit = {
    disc.resetToZero()
    cont.resetToZero()
    vect.resetToZero()
    mats.resetToZero()
  }

  def randomize(eps: => Double):setting.type = {
    cont.randomize(eps)
    vect.randomize(eps)
    mats.randomize(eps)
    setting
  }

  final class DiscBuffer(val length: Int) extends Buffer[Int](setting) {
    def resetToZero(offset: Int) = array(offset) = 0

    def randomize(eps: => Double) = {}
  }

  final class ContBuffer(val length: Int) extends Buffer[Double](setting) {
    def *=(scale: Double): Unit = {
      for (i <- 0 until length) array(i) *= scale
      flagAllChanged()
    }

    def :=(scale: Double): Unit = {
      for (i <- 0 until length) array(i) = scale
      flagAllChanged()
    }

    def +=(that: Buffer[Double]): Unit = {
      for (i <- 0 until length) array(i) += that(i)
      flagAllChanged()
    }

    def addIfChanged(that: Buffer[Double]): Unit = {
      for (i <- that.changed()) {
        array(i) += that(i)
      }
      addChanges(that.changed())
    }

    def resetToZero(offset: Int) = array(offset) = 0.0

    def randomize(eps: => Double) = {
      for (i <- 0 until length) array(i) = array(i) + eps
      flagAllChanged()
    }
  }

  final class VectBuffer(val length: Int) extends Buffer[FactorieVector](setting) {
    def *=(scale: Double): Unit = {
      for (i <- 0 until length) array(i) *= scale
      flagAllChanged()
    }

    def :=(scale: Double): Unit = {
      flagAllChanged()
      for (i <- 0 until length) array(i) := scale
    }

    override def :=(that: Buffer[FactorieVector]): Unit = {
      flagAllChanged()
      for (i <- 0 until length) this(i) = that(i)
    }

    def +=(that: Buffer[FactorieVector]): Unit = {
      flagAllChanged()
      for (i <- 0 until length) add(i, that(i))
    }

    def addIfChanged(that: Buffer[FactorieVector]): Unit = {
      for (i <- that.changed()) {
        add(i, that(i))
      }
      addChanges(that.changed())
    }

    def randomize(eps: => Double) = {
      for (i <- 0 until length)
        for (j <- 0 until array(i).length)
          array(i)(j) += eps
      flagAllChanged()
    }

    def resetToZero(offset: Int) = array(offset).zero() // := 0

    override def update(index: Int, value: FactorieVector): Unit = {
      if (this(index) == null) {
        super.update(index, value.copy)
      } else {
        if (adaptiveVectors) {
          (this(index), value) match {
            case (_: DenseTensor1, target: SparseIndexedTensor1) =>
              super.update(index, target.copy)
            case (_: SparseIndexedTensor1, target: DenseTensor1) =>
              super.update(index, target.copy)
            case (_, _) =>
              this(index) := value
          }
        } else {
          this(index) := value
          recordChange(index)
        }
      }
    }


    def set(index: Int, value: FactorieVector, scale: Double): Unit = {
      update(index, value)
      this(index) *= scale
    }


    def add(index: Int, value: FactorieVector): Unit = {
      if (adaptiveVectors) {
        if (this(index) == null) {
          this(index) = value.copy
        } else {
          (this(index), value) match {
            case (current: SparseIndexedTensor1, arg: DenseTensor1) =>
              this(index) = arg.copy
              this(index) += current
            case (current: DenseTensor1, arg: SparseIndexedTensor1) =>
              this(index) = arg.copy
              this(index) += current
            case (_, _) =>
              this(index) += value
          }
        }
      } else {
        this(index) += value
      }
    }


  }

  final class MatrixBuffer(val length: Int) extends Buffer[FactorieMatrix](setting) {
    def *=(scale: Double): Unit = {
      for (i <- 0 until length) array(i) *= scale
      flagAllChanged()
    }

    def :=(scale: Double): Unit = {
      for (i <- 0 until length) array(i) := scale
      flagAllChanged()
    }

    def +=(that: Buffer[FactorieMatrix]): Unit = {
      for (i <- 0 until length) array(i) += that(i)
      flagAllChanged()
    }

    def addIfChanged(that: Buffer[FactorieMatrix]): Unit = {
      for (i <- that.changed()) {
        array(i) += that(i)
      }
      addChanges(that.changed())
    }

    override def :=(that: Buffer[FactorieMatrix]): Unit = {
      flagAllChanged()
      for (i <- 0 until length) this(i) = that(i)
    }


    override def update(index: Int, value: FactorieMatrix): Unit = {
      if (this(index) == null) {
        super.update(index, value.copy)
      } else {
        this(index) := value
        recordChange(index)
      }

    }


    def resetToZero(offset: Int) = array(offset) := 0

    def randomize(eps: => Double) = {
      for (i <- 0 until length)
        for (j <- 0 until array(i).length)
          array(i)(j) += eps
      flagAllChanged()
    }

  }

  private var adaptiveVectors = false
  var recordChangedOffsets = false

  def setAdaptiveVectors(adaptive: Boolean): Unit = {
    adaptiveVectors = adaptive
  }

  def copyTo(target: Setting, targetOffsets: Offsets, targetMultiplier: Int): Unit = {
    disc.copyTo(target.disc, 0, targetOffsets.discOff * targetMultiplier, disc.length)
    cont.copyTo(target.cont, 0, targetOffsets.contOff * targetMultiplier, cont.length)
    vect.copyTo(target.vect, 0, targetOffsets.vectOff * targetMultiplier, vect.length)
    mats.copyTo(target.mats, 0, targetOffsets.matsOff * targetMultiplier, mats.length)
  }

  def copyTo(target: Setting, srcOffsets: Offsets, targetOffsets: Offsets, length: Offsets): Unit = {
    disc.copyTo(target.disc, srcOffsets.discOff, targetOffsets.discOff, length.discOff)
    cont.copyTo(target.cont, srcOffsets.contOff, targetOffsets.contOff, length.contOff)
    vect.copyTo(target.vect, srcOffsets.vectOff, targetOffsets.vectOff, length.vectOff)
    mats.copyTo(target.mats, srcOffsets.matsOff, targetOffsets.matsOff, length.matsOff)
  }

  def copyTo(target: Setting, srcElementLength: Offsets, srcMultiplier: Int, tgtElementLength: Offsets, tgtMultiplier: Int,
             length: Offsets, srcOffsets: Offsets = Offsets(), tgtOffsets: Offsets = Offsets()): Unit = {
    disc.copyTo(target.disc, srcOffsets.discOff + length.discOff * srcMultiplier,
      tgtOffsets.discOff + tgtElementLength.discOff * tgtMultiplier, length.discOff)
    cont.copyTo(target.cont, srcOffsets.contOff + length.contOff * srcMultiplier,
      tgtOffsets.contOff + tgtElementLength.contOff * tgtMultiplier, length.contOff)
    vect.copyTo(target.vect, srcOffsets.vectOff + length.vectOff * srcMultiplier,
      tgtOffsets.vectOff + tgtElementLength.vectOff * tgtMultiplier, length.vectOff)
    mats.copyTo(target.mats, srcOffsets.matsOff + length.matsOff * srcMultiplier,
      tgtOffsets.matsOff + tgtElementLength.matsOff * tgtMultiplier, length.matsOff)
  }


  def *=(scale: Double): Unit = {
    cont *= scale
    vect *= scale
    mats *= scale
  }

  def +=(that: Setting): Unit = {
    disc := that.disc //todo: this is odd but required to pass on discrete values in gradients
    cont += that.cont
    vect += that.vect
    mats += that.mats
  }

  def addIfChanged(that: Setting): Unit = {
    disc := that.disc //todo: this is odd but required to pass on discrete values in gradients
    cont.addIfChanged(that.cont)
    vect.addIfChanged(that.vect)
    mats.addIfChanged(that.mats)
  }


  def :=(value: Double = 0.0): Unit = {
    cont := value
    vect := value
    mats := value
  }

  def :=(that: Setting): Unit = {
    disc := that.disc
    cont := that.cont
    vect := that.vect
    mats := that.mats
  }

  def :=(src: Setting, srcOffsets: Offsets, lengths: Offsets): Unit = {
    disc :=(src.disc, srcOffsets.discOff, lengths.discOff)
    cont :=(src.cont, srcOffsets.contOff, lengths.contOff)
    vect :=(src.vect, srcOffsets.vectOff, lengths.vectOff)
    mats :=(src.mats, srcOffsets.matsOff, lengths.matsOff)
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
    s"""
       |${disc.mkString(" ")}
        |${cont.mkString(" ")}
        |${vect.mkString(" ")}
        |${mats.mkString(" ")}
     """.stripMargin
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

  def linkedSettings(from: Seq[Var[Dom]], to: Seq[Var[Dom]]): Settings = {
    val mapping = VariableMapping(from, to)
    val result = new Settings(to.length)
    mapping.linkTargetsToSource(this, result)
    result
  }

  def :=(that:Settings): Unit = {
    for (i <- 0 until length) {
      this(i) := that(i)
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

class VectMsg(var mean: FactorieVector = null)

class MatsMsg(var mean: FactorieMatrix = null)


class Msg(numDisc: Int = 0, numCont: Int = 0, numVect: Int = 0, numMats: Int = 0) {
  import MoreArrayOps._
  final var disc = Array.ofDim[DiscMsg](numDisc)
  final var cont = Array.ofDim[ContMsg](numCont)
  final var vect = Array.ofDim[VectMsg](numVect)
  final var mats = Array.ofDim[MatsMsg](numMats)

  final def :=(value: Double): Unit = {
    for (i <- 0 until disc.length) util.Arrays.fill(disc(i).msg, value)
    for (i <- 0 until cont.length) cont(i).mean = value
    for (i <- 0 until vect.length) if (vect(i) != null) vect(i).mean := value
    for (i <- 0 until mats.length) if (mats(i) != null) mats(i).mean := value
  }

  final def +=(value: Msg): Unit = {
    for (i <- 0 until disc.length) {
      incr(value.disc(i).msg,disc(i).msg)
    }
    for (i <- 0 until cont.length) {
      cont(i).mean += value.cont(i).mean
    }
  }

  def argmax(target:Setting): Unit = {
    target.disc.resetToZero()
    for (i <- 0 until disc.length) {
      val argmaxIndex = maxIndex(disc(i).msg)
      target.disc(i) = argmaxIndex
    }
    for (i <- 0 until cont.length) {
      target.cont(i) = cont(i).mean
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
  def apply(msgs:Seq[Msg]) = {
    val result =new Msgs(msgs.length)
    for ((msg,i) <- msgs.zipWithIndex) result(i) = msg
    result
  }
}


final class VariableMapping(val srcIndex: Array[Int], val tgtIndex: Array[Int]) {

  def length = srcIndex.length

  lazy val pairs = srcIndex zip tgtIndex

  def copyForwardDeep(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) tgt(tgtIndex(i)) := src(srcIndex(i))
  }

  def copyForwardDeep(src: Settings, tgt: Settings) = {
    for (i <- 0 until srcIndex.length)
      tgt(tgtIndex(i)) := src(srcIndex(i))
  }


  def copyForwardShallow(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) tgt(tgtIndex(i)) = src(srcIndex(i))
  }

  def linkTargetsToSource(src: Settings, tgt: Settings) = {
    for (i <- 0 until srcIndex.length) tgt(tgtIndex(i)) = src(srcIndex(i))
  }


  def copyBackwardDeep(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) src(srcIndex(i)) := tgt(tgtIndex(i))
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

abstract class Buffer[T: ClassTag](val setting: Setting) {
  def length: Int

  lazy val changedIndices = new mutable.HashSet[Int]
  protected var allChanged = false

  def resetChanges() = {
    changedIndices.clear()
    allChanged = false
  }

  def addChanges(indices: Iterable[Int]): Unit = {
    if (shouldRecord) changedIndices ++= indices
  }

  def resetToZero(): Unit = {
    for (i <- changed()) resetToZero(i)
    changedIndices.clear()
    allChanged = false
  }

  def resetToZero(offset: Int): Unit


  protected def flagAllChanged() {
    allChanged = true
  }

  def shouldRecord = setting.recordChangedOffsets && !allChanged

  def recordChange(offset: Int): Unit = {
    if (shouldRecord) changedIndices += offset
  }

  val array = Array.ofDim[T](length)

  def changed() = {
    if (!setting.recordChangedOffsets || allChanged) Range(0, length) else changedIndices
  }

  def update(index: Int, value: T) = {
    array(index) = value
    if (shouldRecord) changedIndices += index
  }

  def foreach(f: T => Unit) = (0 until length).foreach(x => f(apply(x)))

  def apply(index: Int) = array(index)

  def copyTo(tgt: Buffer[T], srcPos: Int, tgtPos: Int, length: Int) = {
    if (length > 0) {
      if (!shouldRecord) {
        System.arraycopy(array, srcPos, tgt.array, tgtPos, length)
        if (tgt.shouldRecord)
          tgt.changedIndices ++= Range(tgtPos, tgtPos + length)
      } else {
        for (i <- changed() if i >= srcPos && i < srcPos + length) {
          tgt(tgtPos + i - srcPos) = this(i)
          if (tgt.shouldRecord) tgt.changedIndices += tgtPos + i - srcPos
        }
      }
    }
  }

  def :=(value: Buffer[T]): Unit = {
    System.arraycopy(value.array, 0, array, 0, length)
    flagAllChanged()
  }

  def :=(src: Buffer[T], srcOffset: Int, srcLength: Int): Unit = {
    System.arraycopy(src.array, srcOffset, array, 0, srcLength)
    flagAllChanged()
  }

  def randomize(eps: =>Double)

  def mkString(sep: String) = array.mkString(sep)
}


object VariableMapping {
  def apply(src: Seq[Var[Dom]], tgt: Seq[Var[Dom]]) = {
    val pairs = src.indices.view.map(i => i -> tgt.indexOf(src(i))).filter(_._2 != -1).toArray
    val (srcIndex, tgtIndex) = (pairs.map(_._1), pairs.map(_._2))
    new VariableMapping(srcIndex, tgtIndex)
  }
}

case class Dimensions(discDims:Array[Range] = Array.empty) {
  def +(that:Dimensions) = copy(discDims ++ that.discDims)
  def *(times:Int) = {
    val result = Array.ofDim[Range](discDims.length * times)
    for (i <- 0 until times)
      System.arraycopy(discDims,0,result,i * discDims.length,discDims.length)
    Dimensions(result)
  }
}

case class Offsets(discOff: Int = 0, contOff: Int = 0, vectOff: Int = 0, matsOff: Int = 0) {
  def +(disc: Int, cont: Int, vect: Int, mats: Int) = Offsets(discOff + disc, contOff + cont, vectOff + vect, matsOff + mats)

  def +(that: Offsets, scale: Int = 1) =
    Offsets(discOff + scale * that.discOff, contOff + scale * that.contOff, vectOff + scale * that.vectOff, matsOff + scale * that.matsOff)

  def *(scale: Int) = Offsets(scale * discOff, scale * contOff, scale * vectOff, scale * matsOff)
}

object Offsets {
  val zero = new Offsets()
}


case class Ranges(from: Offsets, to: Offsets) {
  def copy(src: Setting, tgt: Setting): Unit = {
    src.disc.copyTo(tgt.disc, from.discOff, 0, to.discOff - from.discOff)
    src.cont.copyTo(tgt.cont, from.contOff, 0, to.contOff - from.contOff)
    src.vect.copyTo(tgt.vect, from.vectOff, 0, to.vectOff - from.vectOff)
    src.mats.copyTo(tgt.mats, from.matsOff, 0, to.matsOff - from.matsOff)
  }

  def addInto(src: Setting, tgt: Setting): Unit = {
    for (i <- 0 until numDisc) {
      tgt.disc(from.contOff + i) = src.disc(i)
    }
    for (i <- 0 until numCont) {
      tgt.cont(from.contOff + i) += src.cont(i)
    }

    for (i <- 0 until numVect) {
      tgt.vect.add(from.vectOff + i, src.vect(i))
    }

    for (i <- 0 until numMats) {
      tgt.mats(from.matsOff + i) += src.mats(i)
    }
  }

  def addIntoIfChanged(src: Setting, tgt: Setting): Unit = {
    for (i <- src.disc.changed()) {
      tgt.disc(from.contOff + i) = src.disc(i)
    }
    for (i <- src.cont.changed()) {
      tgt.cont(from.contOff + i) += src.cont(i)
    }

    for (i <- src.vect.changed()) {
      tgt.vect.add(from.vectOff + i, src.vect(i))
    }

    for (i <- src.mats.changed()) {
      tgt.mats(from.matsOff + i) += src.mats(i)
    }
  }


  def numDisc = to.discOff - from.discOff

  def numCont = to.contOff - from.contOff

  def numVect = to.vectOff - from.vectOff

  def numMats = to.matsOff - from.matsOff
}



