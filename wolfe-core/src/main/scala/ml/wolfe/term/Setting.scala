package ml.wolfe.term

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

  //fixme: value isn't used!?
  final def :=(value: Double): Unit = {
    for (i <- 0 until cont.length) cont(i) = 0.0
    for (i <- 0 until vect.length) if (vect(i) != null) vect(i) := 0.0
    for (i <- 0 until mats.length) if (mats(i) != null) mats(i) := 0.0
  }

  final def :=(that: Setting): Unit = {
    for (i <- 0 until disc.length) disc(i) = that.disc(i)
    for (i <- 0 until cont.length) cont(i) = that.cont(i)
    for (i <- 0 until vect.length) vect(i) = that.vect(i)
    for (i <- 0 until mats.length) mats(i) = that.mats(i)
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


class DiscMsg(dim:Int) {
  var msg = Array.ofDim[Double](dim)
}
class ContMsg {
  var mean:Double = 0.0
}
class VectMsg {
  var mean:FactorieVector = null
}

class Msgs(val disc:Array[DiscMsg],
           val cont:Array[ContMsg],
           val vect:Array[VectMsg])





