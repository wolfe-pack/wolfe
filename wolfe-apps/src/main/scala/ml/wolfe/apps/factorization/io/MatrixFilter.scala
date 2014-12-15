package ml.wolfe.apps.factorization.io

import java.io.FileWriter

import ml.wolfe.apps.factorization.TensorKB
import ml.wolfe.util.Conf

/**
 * Reads in a huge sparse matrix and filters it.
 * Can also be used to add more data to the matrix (e.g. freebase facts).
 * @author rockt
 */
object MatrixFilter extends App {
  val filePath = args.lift(0).getOrElse("./data/bbc/matrix_multi_all.txt")
  println("Loading...")
  val kb = LoadTSV(filePath = filePath)
  println(kb.toInfoString)

  val matricesToAdd = if (args.size > 2) args.tail.tail else Array(
    "./data/bbc/matrix_freebase.txt"
  )
  println("Loading additional data...")
  matricesToAdd.foreach(fileName => LoadTSV(db = kb, filePath = fileName))
  println(kb.toInfoString)

  println("Filtering...")
  val filteredKB = new TensorKB()

  val frequentRows = kb.keys2.filter(key2 => kb.getBy2(key2).size > 10).toSet
  val frequentCols = kb.keys1.filter(key1 => kb.getBy1(key1).size > 25).toSet

  val filteredCells =
    kb.cells.filter(c => frequentCols(c.key1) && frequentRows(c.key2))
    .foreach(cell => filteredKB += cell)

  println(filteredKB.toInfoString)

  val fileWriter = new FileWriter(args.lift(1).getOrElse("./data/bbc/matrix_final.txt"))
  filteredKB.cells.foreach(cell => {
    val (e1, e2) = cell.key2
    fileWriter.write(s"${cell.key1}\t$e1\t$e2\t${cell.cellType}\t${cell.target}\n")
  })
  fileWriter.close()
}

/**
 * Shows stats about the matrix, e.g., what freebase relations are in there.
 */
object MatrixInspector extends App {
  val kb = LoadTSV(filePath = args.lift(0).getOrElse("./data/bbc/matrix_final.txt"))
  println(kb.toInfoString)

  val freebaseRelations = kb.keys1.filter(_.toString.startsWith("REL$"))
  freebaseRelations.foreach(println)
}