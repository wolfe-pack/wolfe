package ml.wolfe.apps.factorization.io

import java.io.FileWriter

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

  val frequentRows = kb.keys2.filter(key2 => kb.getBy2(key2).size > 5).toSet
  val frequentCols = kb.keys1.filter(key1 => kb.getBy1(key1).size > 10).toSet

  val filteredCells =
    kb.cells.filter(c => frequentCols(c.key1) && frequentRows(c.key2))
    .foreach(cell => filteredKB += cell)

  println(filteredKB.toInfoString)

  val fileWriter = new FileWriter(args.lift(1).getOrElse("./data/bbc/matrix_filtered.txt"))
  filteredKB.cells.foreach(cell => {
    val (e1, e2) = cell.key2
    fileWriter.write(s"${cell.key1}\t$e1\t$e2\t${cell.cellType}\t${cell.target}\n")
  })
  fileWriter.close()
}
