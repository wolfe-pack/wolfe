package ml.wolfe.apps.factorization.io

import ml.wolfe.apps.factorization.{DefaultIx, Cell, CellType, TensorKB}
import ml.wolfe.util.{ProgressBar, Conf}

import scala.io.Source
import scala.util.Random

/**
 * @author rockt
 */
object LoadTSV extends App {
  def apply(k: Int = 100, subsample: Double = 1.0, db: TensorKB = null, filePath: String = Conf.getString("inputFile")): TensorKB = {
    val kb = if (db != null) db else new TensorKB(k)
    val rand = new Random(0l)

    val lines = Source.fromFile(filePath).getLines()

    val progressBar = new ProgressBar(Source.fromFile(filePath).getLines().size, 100000)
    progressBar.start()

    for {
      fact <- lines
      Array(r, e1, e2, typ, target) = fact.split("\t")
    } {
      val cellType = typ match {
        case "Train" => CellType.Train
        case "Test" => CellType.Test
        case "Dev" => CellType.Dev
        case "Observed" => CellType.Observed
      }

      if (rand.nextDouble() < subsample) {
        val cell = Cell(r, (e1, e2), DefaultIx, target.toDouble, cellType)
        kb += cell
      }

      progressBar(r)
    }

    kb
  }
}
