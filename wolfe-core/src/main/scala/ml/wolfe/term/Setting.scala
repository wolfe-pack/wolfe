package ml.wolfe.term

import ml.wolfe._
import ml.wolfe.fg20.{ArgMap, PartialSetting}
import java.util


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

  final def fillObserved(observed: PartialSetting) = {
    for (i <- 0 until disc.length; if observed.discObs(i)) disc(i) = observed.disc(i)
    for (i <- 0 until cont.length; if observed.contObs(i)) cont(i) = observed.cont(i)
    for (i <- 0 until vect.length; if observed.vectObs(i)) vect(i) = observed.vect(i)
    //for (i <- 0 until mats.length; if observed.matsObs(i)) mats(i) = observed.mats(i)
  }

  final def copyFrom(that: Setting, mapFromThisIndextoThatIndex: ArgMap): Unit = {
    for (i <- 0 until disc.length) disc(i) = that.disc(mapFromThisIndextoThatIndex.tgtDisc(i))
    for (i <- 0 until cont.length) cont(i) = that.cont(mapFromThisIndextoThatIndex.tgtCont(i))
    for (i <- 0 until vect.length) vect(i) = that.vect(mapFromThisIndextoThatIndex.tgtVect(i))
    //for (i <- 0 until mats.length) mats(i) = that.mats(mapFromThisIndextoThatIndex.tgtMats(i))
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

  final def copyTo(that: Setting, mapFromThisIndextoThatIndex: ArgMap): Unit = {
    for (i <- 0 until disc.length) that.disc(mapFromThisIndextoThatIndex.tgtDisc(i)) = disc(i)
    for (i <- 0 until cont.length) that.cont(mapFromThisIndextoThatIndex.tgtCont(i)) = cont(i)
    for (i <- 0 until vect.length) that.vect(mapFromThisIndextoThatIndex.tgtVect(i)) = vect(i)
    //for (i <- 0 until mats.length) that.mats(mapFromThisIndextoThatIndex.tgtMats(i)) = mats(i)
  }

  final def observeIn(that: PartialSetting, mapFromThisIndextoThatIndex: ArgMap, observed: Boolean = true): Unit = {
    for (i <- 0 until disc.length) that.discObs(mapFromThisIndextoThatIndex.tgtDisc(i)) = observed
    for (i <- 0 until cont.length) that.contObs(mapFromThisIndextoThatIndex.tgtCont(i)) = observed
    for (i <- 0 until vect.length) that.vectObs(mapFromThisIndextoThatIndex.tgtVect(i)) = observed
    //for (i <- 0 until mats.length) that.matsObs(mapFromThisIndextoThatIndex.tgtMats(i)) = observed
  }

  final def inverseObserveIn(that: PartialSetting, mapFromThisIndextoThatIndex: ArgMap, observed: Boolean = true): Unit = {
    util.Arrays.fill(that.discObs, observed)
    util.Arrays.fill(that.contObs, observed)
    util.Arrays.fill(that.vectObs, observed)
    //util.Arrays.fill(that.matsObs, observed)
    for (i <- 0 until disc.length) that.discObs(mapFromThisIndextoThatIndex.tgtDisc(i)) = !observed
    for (i <- 0 until cont.length) that.contObs(mapFromThisIndextoThatIndex.tgtCont(i)) = !observed
    for (i <- 0 until vect.length) that.vectObs(mapFromThisIndextoThatIndex.tgtVect(i)) = !observed
    //for (i <- 0 until mats.length) that.matsObs(mapFromThisIndextoThatIndex.tgtMats(i)) = !observed
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
