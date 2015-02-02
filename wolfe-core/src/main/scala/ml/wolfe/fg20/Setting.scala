package ml.wolfe.fg20

import ml.wolfe.term.Setting

import scala.collection.mutable.ListBuffer

/**
 * @author riedel
 */



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