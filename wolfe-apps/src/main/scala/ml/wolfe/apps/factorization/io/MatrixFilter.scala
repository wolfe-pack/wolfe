package ml.wolfe.apps.factorization.io

import ml.wolfe.apps.factorization.TensorKB
import ml.wolfe.util.Conf

/**
 * @author rockt
 */
object MatrixFilter extends App {
  val confPath = args.lift(0).getOrElse("./conf/mf-hack.conf")

  Conf.add(confPath)
  Conf.outDir //sets up output directory
  implicit val conf = Conf
  println("Using " + confPath)
  println("Loading...")

  val kb = LoadTSV()

  println(kb.toInfoString)

  val filteredKB = new TensorKB()



}
