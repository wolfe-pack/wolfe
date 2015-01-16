package ml.wolfe.fg20

import java.util

import ml.wolfe._

import scala.collection.mutable.ListBuffer

/**
 * @author riedel
 */
/**
 * A setting of a clique of discrete, continuous and vector variables.
 * @param numDisc number of discrete assignments.
 * @param numCont number of continuous assignments.
 * @param numVect number of vector assignments.
 */
class Setting(numDisc: Int, numCont: Int = 0, numVect: Int = 0) {
  final var disc = Array.ofDim[Int](numDisc)
  final var cont = Array.ofDim[Double](numCont)
  final var vect = Array.ofDim[FactorieVector](numVect)


  final def fillObserved(observed: PartialSetting) = {
    for (i <- 0 until disc.length; if observed.discObs(i)) disc(i) = observed.disc(i)
    for (i <- 0 until cont.length; if observed.contObs(i)) cont(i) = observed.cont(i)
    for (i <- 0 until vect.length; if observed.vectObs(i)) vect(i) = observed.vect(i)
  }

  final def copyFrom(that: Setting, mapFromThisIndextoThatIndex: ArgMap): Unit = {
    for (i <- 0 until disc.length) disc(i) = that.disc(mapFromThisIndextoThatIndex.discArgs(i))
    for (i <- 0 until cont.length) cont(i) = that.cont(mapFromThisIndextoThatIndex.contArgs(i))
    for (i <- 0 until vect.length) vect(i) = that.vect(mapFromThisIndextoThatIndex.vectArgs(i))
  }

  final def *=(scale: Double): Unit = {
    for (i <- 0 until cont.length) cont(i) *= scale
    for (i <- 0 until vect.length) vect(i) *= scale
  }
  final def :=(value: Double): Unit = {
    for (i <- 0 until cont.length) cont(i) = 0.0
    for (i <- 0 until vect.length) if (vect(i) != null) vect(i) := 0.0
  }

  final def copyTo(that: Setting, mapFromThisIndextoThatIndex: ArgMap): Unit = {
    for (i <- 0 until disc.length) that.disc(mapFromThisIndextoThatIndex.discArgs(i)) = disc(i)
    for (i <- 0 until cont.length) that.cont(mapFromThisIndextoThatIndex.contArgs(i)) = cont(i)
    for (i <- 0 until vect.length) that.vect(mapFromThisIndextoThatIndex.vectArgs(i)) = vect(i)
  }

  final def observeIn(that: PartialSetting, mapFromThisIndextoThatIndex: ArgMap, observed: Boolean = true): Unit = {
    for (i <- 0 until disc.length) that.discObs(mapFromThisIndextoThatIndex.discArgs(i)) = observed
    for (i <- 0 until cont.length) that.contObs(mapFromThisIndextoThatIndex.contArgs(i)) = observed
    for (i <- 0 until vect.length) that.vectObs(mapFromThisIndextoThatIndex.vectArgs(i)) = observed
  }

  final def inverseObserveIn(that: PartialSetting, mapFromThisIndextoThatIndex: ArgMap, observed: Boolean = true): Unit = {
    util.Arrays.fill(that.discObs, observed)
    util.Arrays.fill(that.contObs, observed)
    util.Arrays.fill(that.vectObs, observed)
    for (i <- 0 until disc.length) that.discObs(mapFromThisIndextoThatIndex.discArgs(i)) = !observed
    for (i <- 0 until cont.length) that.contObs(mapFromThisIndextoThatIndex.contArgs(i)) = !observed
    for (i <- 0 until vect.length) that.vectObs(mapFromThisIndextoThatIndex.vectArgs(i)) = !observed
  }

  def epsEquals(eps: Double, that: Setting): Boolean = {
    if (disc.length != that.disc.length) return false
    if (cont.length != that.cont.length) return false
    if (vect.length != that.vect.length) return false

    for (i <- 0 until disc.length) if (disc(i) != that.disc(i)) return false
    for (i <- 0 until cont.length) if (math.abs(cont(i) - that.cont(i)) > eps) return false
    for (i <- 0 until vect.length; j <- 0 until vect(i).size) if (math.abs(vect(i)(j) - that.vect(i)(j)) > eps) return false
    true
  }


}
/**
 * A partial setting of a clique. The only observed or set values are
 * those at the indices for which the corresponding *Obs array returns true.
 * @param numDisc number of discrete assignments.
 * @param numCont number of continuous assignments.
 * @param numVect number of vector assignments.
 */
final class PartialSetting(numDisc: Int, numCont: Int = 0, numVect: Int = 0) extends Setting(numDisc, numCont, numVect) {

  var discObs = Array.ofDim[Boolean](numDisc)
  var contObs = Array.ofDim[Boolean](numCont)
  var vectObs = Array.ofDim[Boolean](numVect)

  private def toMapping(observed: Array[Boolean]) = {
    val result = new ListBuffer[Int]
    var offset = 0
    for (i <- 0 until observed.length) {
      if (observed(i)) offset += 1 else result += i
    }
    result.toArray
  }

  def copyObservedTo(that: PartialSetting, mapFromThisIndextoThatIndex: ArgMap): Unit = {
    for (i <- 0 until disc.length; if discObs(i)) {
      that.disc(mapFromThisIndextoThatIndex.discArgs(i)) = disc(i)
      that.discObs(i) = true
    }
    for (i <- 0 until cont.length; if contObs(i)) {
      that.cont(mapFromThisIndextoThatIndex.contArgs(i)) = cont(i)
      that.contObs(i) = true
    }
    for (i <- 0 until vect.length; if vectObs(i)) {
      that.vect(mapFromThisIndextoThatIndex.vectArgs(i)) = vect(i)
      that.vectObs(i)
    }
  }

  def copyObservedTo(that: PartialSetting): Unit = {
    for (i <- 0 until disc.length; if discObs(i)) {
      that.disc(i) = disc(i)
      that.discObs(i) = true
    }
    for (i <- 0 until cont.length; if contObs(i)) {
      that.cont(i) = cont(i)
      that.contObs(i) = true
    }
    for (i <- 0 until vect.length; if vectObs(i)) {
      that.vect(i) = vect(i)
      that.vectObs(i)
    }
  }


  def fromObservedToAllMapping() = {
    new ArgMap(toMapping(discObs), toMapping(contObs), toMapping(vectObs))
  }

}


/**
 * Class to store double results in.
 * @param value the value to store.
 */
final class DoubleBuffer(var value: Double = 0.0)

/**
 * Class that represents a mapping from variable indices in one clique to indices in another clique
 */
class ArgMap(val discArgs: Array[Int], val contArgs: Array[Int], val vectArgs: Array[Int])

object ArgMap {
  def offset(clique: Clique, discOffset: Int = 0, contOffset: Int = 0, vectOffset: Int = 0) = {
    new ArgMap(
      clique.discVars.indices.map(_ + discOffset).toArray,
      clique.contVars.indices.map(_ + contOffset).toArray,
      clique.vectVars.indices.map(_ + vectOffset).toArray)
  }
}