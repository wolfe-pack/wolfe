package ml.wolfe.term

import java.lang.System._
import java.util

import cc.factorie.la.{DenseTensor1, SparseTensor1}
import ml.wolfe.{FactorieMatrix, FactorieVector}


/**
 * A setting of a clique of discrete, continuous and vector variables.
 * @param numDisc number of discrete assignments.
 * @param numCont number of continuous assignments.
 * @param numVect number of vector assignments.
 */
class Setting(numDisc: Int = 0, numCont: Int = 0, numVect: Int = 0, numMats: Int = 0) {
  final var disc = Array.ofDim[Int](numDisc)
  final var cont = Array.ofDim[Double](numCont)
  final var vect = Array.ofDim[FactorieVector](numVect)
  final var mats = Array.ofDim[FactorieMatrix](numMats)

  final def copyTo(target: Setting, targetOffsets: Offsets, length: Int = 1): Unit = {
    if (disc.length > 0) System.arraycopy(disc, 0, target.disc, targetOffsets.discOff * length, disc.length)
    if (cont.length > 0) System.arraycopy(cont, 0, target.cont, targetOffsets.contOff * length, cont.length)
    if (vect.length > 0) System.arraycopy(vect, 0, target.vect, targetOffsets.vectOff * length, vect.length)
    if (mats.length > 0) System.arraycopy(mats, 0, target.mats, targetOffsets.matsOff * length, mats.length)
  }

  final def *=(scale: Double): Unit = {
    for (i <- 0 until cont.length) cont(i) *= scale
    for (i <- 0 until vect.length) vect(i) *= scale
    for (i <- 0 until mats.length) mats(i) *= scale
  }

  final def +=(that: Setting): Unit = {
    for (i <- 0 until cont.length) cont(i) += that.cont(i)
    for (i <- 0 until vect.length) vect(i) += that.vect(i)
    for (i <- 0 until mats.length) mats(i) += that.mats(i)
  }

  final def :=(value: Double = 0.0): Unit = {
    for (i <- 0 until cont.length) cont(i) = value
    for (i <- 0 until vect.length) if (vect(i) != null) vect(i) := value
    for (i <- 0 until mats.length) if (mats(i) != null) mats(i) := value
  }

  final def :=(that: Setting): Unit = {
    for (i <- 0 until disc.length) disc(i) = that.disc(i)
    for (i <- 0 until cont.length) cont(i) = that.cont(i)
    for (i <- 0 until vect.length) vect(i) = that.vect(i)
    for (i <- 0 until mats.length) mats(i) = that.mats(i)
  }

  def ensureSparsity(): Unit = {
    for (i <- 0 until vect.length) {
      vect(i) match {
        case d:DenseTensor1 =>
          vect(i) = new SparseTensor1(d.dim1)
        case _ =>
      }
    }

  }

  final def setVect(index: Int, value: FactorieVector): Unit = {
    if (vect(0) == null) {
      vect(0) = value.copy
    } else {
      (vect(0), value) match {
        case (_: DenseTensor1, target: SparseTensor1) =>
          vect(0) = target.copy
        case (_: SparseTensor1, target: DenseTensor1) =>
          vect(0) = target.copy
        case (_,_) =>
          vect(0) := value
      }
    }
  }

  final def setVect(index: Int, value: FactorieVector, scale:Double): Unit = {
    setVect(index,value)
    vect(0) *= scale
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


class DiscMsg(var msg: Array[Double]) {
  def this(size: Int) = this(Array.ofDim[Double](size))
}

class ContMsg(var mean: Double = 0.0)

class VectMsg(var mean: FactorieVector = null)

class MatsMsg(var mean: FactorieMatrix = null)


class Msgs(numDisc: Int = 0, numCont: Int = 0, numVect: Int = 0, numMats: Int = 0) {
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


}

final class VariableMapping(val srcIndex: Array[Int], val tgtIndex: Array[Int]) {
  def copyForwardDeep(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) tgt(tgtIndex(i)) := src(srcIndex(i))
  }

  def copyForwardShallow(src: Array[Setting], tgt: Array[Setting]) = {
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

object VariableMapping {
  def apply(src: Seq[Var[Dom]], tgt: Seq[Var[Dom]]) = {
    val pairs = src.indices.view.map(i => i -> tgt.indexOf(src(i))).filter(_._2 != -1).toArray
    val (srcIndex, tgtIndex) = (pairs.map(_._1), pairs.map(_._2))
    new VariableMapping(srcIndex, tgtIndex)
  }
}

case class Offsets(discOff: Int = 0, contOff: Int = 0, vectOff: Int = 0, matsOff: Int = 0) {
  def +(disc: Int, cont: Int, vect: Int, mats: Int) = Offsets(discOff + disc, contOff + cont, vectOff + vect, matsOff + mats)

  def +(that: Offsets, scale: Int = 1) =
    Offsets(discOff + scale * that.discOff, contOff + scale * that.contOff, vectOff + scale * that.vectOff, matsOff + scale * that.matsOff)

  def *(scale: Int) = Offsets(scale * discOff, scale * contOff, scale * vectOff, scale * matsOff)
}

case class Ranges(from: Offsets, to: Offsets) {
  def copy(src: Setting, tgt: Setting): Unit = {
    arraycopy(src.disc, from.discOff, tgt.disc, 0, to.discOff - from.discOff)
    arraycopy(src.cont, from.contOff, tgt.cont, 0, to.contOff - from.contOff)
    arraycopy(src.vect, from.vectOff, tgt.vect, 0, to.vectOff - from.vectOff)
    arraycopy(src.mats, from.matsOff, tgt.mats, 0, to.matsOff - from.matsOff)
  }

  def addInto(src: Setting, tgt: Setting): Unit = {
    for (i <- 0 until numCont) {
      tgt.cont(from.contOff + i) += src.cont(i)
    }

    for (i <- 0 until numVect) {
      tgt.vect(from.vectOff + i) += src.vect(i)
    }

    for (i <- 0 until numMats) {
      tgt.mats(from.matsOff + i) += src.mats(i)
    }
  }

  def numDisc = to.discOff - from.discOff

  def numCont = to.contOff - from.contOff

  def numVect = to.vectOff - from.vectOff

  def numMats = to.matsOff - from.matsOff
}



