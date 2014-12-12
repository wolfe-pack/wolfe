package ml.wolfe.apps.factorization.io

import ml.wolfe.apps.factorization.{DefaultIx, Cell, CellType, TensorKB}
import ml.wolfe.util.Conf

import scala.io.Source
import scala.util.Random

/**
 * @author rockt
 */
object LoadTSV extends App {
  def apply(k: Int = 100, subsample: Double = 1.0, db: TensorKB = null): TensorKB = {
    val kb = if (db != null) db else new TensorKB(k)
    val rand = new Random(0l)

    val lines = Source.fromFile(Conf.getString("inputFile")).getLines()

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
    }

    kb
  }
}
