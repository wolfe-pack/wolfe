package ml.wolfe.apps.factorization.io

import java.io.File

import ml.wolfe.apps.{CellType, Cell, TensorKB}

/**
 * Created by rockt on 26/10/2014.
 */
object LoadNAACL2013 extends App {
  def apply(): TensorKB = {
    val kb = new TensorKB()

    //val facts = io.Source.fromFile(new File(this.getClass.getResource("/naacl2013.txt").toURI)).getLines()

    val zipFile = new java.util.zip.ZipFile(new File(this.getClass.getResource("/naacl2013.txt.zip").toURI))
    import collection.JavaConverters._

    val List(entry) = zipFile.entries.asScala.toList
    val facts = io.Source.fromInputStream(zipFile.getInputStream(entry)).getLines()

    for {
      fact <- facts
      Array(r, e1, e2, typ, target) = fact.split("\t")
    } {
      val cellType = typ match {
        case "Train" => CellType.Train
        case "Test" => CellType.Test
        case "Dev" => CellType.Dev
        case "Observed" => CellType.Observed
      }
      val cell = Cell(r, e1, e2, target.toDouble, cellType) //todo match for cell type
      kb += cell
    }

    kb
  }

  val tensorKB = apply()

  println(tensorKB.numCells)
}

