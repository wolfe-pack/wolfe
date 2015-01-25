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
    for (i <- 0 until disc.length) disc(i) = that.disc(mapFromThisIndextoThatIndex.tgtDisc(i))
    for (i <- 0 until cont.length) cont(i) = that.cont(mapFromThisIndextoThatIndex.tgtCont(i))
    for (i <- 0 until vect.length) vect(i) = that.vect(mapFromThisIndextoThatIndex.tgtVect(i))
  }

  final def *=(scale: Double): Unit = {
    for (i <- 0 until cont.length) cont(i) *= scale
    for (i <- 0 until vect.length) vect(i) *= scale
  }
  final def :=(value: Double): Unit = {
    for (i <- 0 until cont.length) cont(i) = 0.0
    for (i <- 0 until vect.length) if (vect(i) != null) vect(i) := 0.0
  }

  final def :=(that:Setting): Unit = {
    for (i <- 0 until disc.length) disc(i) = that.disc(i)
    for (i <- 0 until cont.length) cont(i) = that.cont(i)
    for (i <- 0 until vect.length) vect(i) = that.vect(i)
  }

  final def copyTo(that: Setting, mapFromThisIndextoThatIndex: ArgMap): Unit = {
    for (i <- 0 until disc.length) that.disc(mapFromThisIndextoThatIndex.tgtDisc(i)) = disc(i)
    for (i <- 0 until cont.length) that.cont(mapFromThisIndextoThatIndex.tgtCont(i)) = cont(i)
    for (i <- 0 until vect.length) that.vect(mapFromThisIndextoThatIndex.tgtVect(i)) = vect(i)
  }

  final def observeIn(that: PartialSetting, mapFromThisIndextoThatIndex: ArgMap, observed: Boolean = true): Unit = {
    for (i <- 0 until disc.length) that.discObs(mapFromThisIndextoThatIndex.tgtDisc(i)) = observed
    for (i <- 0 until cont.length) that.contObs(mapFromThisIndextoThatIndex.tgtCont(i)) = observed
    for (i <- 0 until vect.length) that.vectObs(mapFromThisIndextoThatIndex.tgtVect(i)) = observed
  }

  final def inverseObserveIn(that: PartialSetting, mapFromThisIndextoThatIndex: ArgMap, observed: Boolean = true): Unit = {
    util.Arrays.fill(that.discObs, observed)
    util.Arrays.fill(that.contObs, observed)
    util.Arrays.fill(that.vectObs, observed)
    for (i <- 0 until disc.length) that.discObs(mapFromThisIndextoThatIndex.tgtDisc(i)) = !observed
    for (i <- 0 until cont.length) that.contObs(mapFromThisIndextoThatIndex.tgtCont(i)) = !observed
    for (i <- 0 until vect.length) that.vectObs(mapFromThisIndextoThatIndex.tgtVect(i)) = !observed
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

object Setting {
  def merge(settings:Array[Setting], result:Setting) = {
    var cont = 0
    var disc = 0
    var vect = 0
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
    }
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
      that.disc(mapFromThisIndextoThatIndex.tgtDisc(i)) = disc(i)
      that.discObs(i) = true
    }
    for (i <- 0 until cont.length; if contObs(i)) {
      that.cont(mapFromThisIndextoThatIndex.tgtCont(i)) = cont(i)
      that.contObs(i) = true
    }
    for (i <- 0 until vect.length; if vectObs(i)) {
      that.vect(mapFromThisIndextoThatIndex.tgtVect(i)) = vect(i)
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
final class ArgMap(val tgtDisc: Array[Int], val tgtCont: Array[Int], val tgtVect: Array[Int])

/**
 * Class that represents a mapping from variable indices in one clique to indices in another clique
 */
final class CliqueMapping(val srcDisc:Array[Int], val srcCont:Array[Int], val srcVect:Array[Int],
                          val tgtDisc:Array[Int], val tgtCont:Array[Int], val tgtVect:Array[Int]){

  def copyForward(src:Setting,tgt:Setting) = {
    for (i <- 0 until srcDisc.length) tgt.disc(tgtDisc(i)) = src.disc(srcDisc(i))
    for (i <- 0 until srcCont.length) tgt.cont(tgtCont(i)) = src.cont(srcCont(i))
    for (i <- 0 until srcVect.length) tgt.vect(tgtVect(i)) = src.vect(srcVect(i))
  }

  def copyBackward(src:Setting,tgt:Setting) = {
    for (i <- 0 until srcDisc.length) src.disc(srcDisc(i)) = tgt.disc(tgtDisc(i))
    for (i <- 0 until srcCont.length) src.cont(srcCont(i)) = tgt.cont(tgtCont(i))
    for (i <- 0 until srcVect.length) src.vect(srcVect(i)) = tgt.vect(tgtVect(i))
  }

}

object CliqueMapping {
  def apply(tgtDisc:Array[Int], tgtCont:Array[Int], tgtVect:Array[Int]) = new CliqueMapping(
    tgtDisc.indices.toArray,tgtCont.indices.toArray,tgtVect.indices.toArray,
    tgtDisc,tgtCont,tgtVect)

  def apply(src:Clique,tgt:Clique) = {
    def mapping(srcVars:Array[_],tgtVars:Array[_]) = {
      val pairs = srcVars.indices.view.map(i => i -> tgtVars.indexOf(src.discVars(i))).filter(_._2 != -1).toArray
      (pairs.map(_._1),pairs.map(_._2))
    }

    val (srcDisc,tgtDisc) = mapping(src.discVars,tgt.discVars)
    val (srcCont,tgtCont) = mapping(src.contVars,tgt.contVars)
    val (srcVect,tgtVect) = mapping(src.vectVars,tgt.vectVars)
    new CliqueMapping(srcDisc,srcCont,srcVect,tgtDisc,tgtCont,tgtVect)
  }

}


object ArgMap {
  def offset(clique: Clique, discOffset: Int = 0, contOffset: Int = 0, vectOffset: Int = 0) = {
    new ArgMap(
      clique.discVars.indices.map(_ + discOffset).toArray,
      clique.contVars.indices.map(_ + contOffset).toArray,
      clique.vectVars.indices.map(_ + vectOffset).toArray)
  }
}