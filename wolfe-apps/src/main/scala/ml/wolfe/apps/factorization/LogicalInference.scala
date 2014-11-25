package ml.wolfe.apps.factorization

import ml.wolfe.apps.factorization.CellType.CellType

/**
 * @author rockt
 */
object LogicalInference {
  def apply(db: TensorDB, formulaList: List[Formula] = Nil, newCellType: CellType = CellType.Train): Unit = {
    var converged = false


    val formulae = if (formulaList.isEmpty) db.formulae.toList else formulaList
    while (!converged) {
      converged = true

      for (formula <- formulae) formula match {
        case Impl(p1, p2, _) =>
          val cs = db.getBy1(p1)
          cs.foreach(c => {
            val (c1, c2) = c
            val cellOpt = db.get(p2, c1, c2)

            if (!cellOpt.isDefined || cellOpt.get.cellType != newCellType) {
              converged = false
              db += Cell(p2, c1, c2, target = 1.0, cellType = newCellType)
            }
          })
        case ImplNeg(p1, p2, _) =>
          val cs = db.getBy1(p1)
          cs.foreach(c => {
            val (c1, c2) = c
            val cellOpt = db.get(p2, c1, c2)

            if (!cellOpt.isDefined || cellOpt.get.cellType != newCellType) {
              converged = false
              db += Cell(p2, c1, c2, target = 0.0, cellType = newCellType)
            }
          })
        case _ => ???
      }
    }

  }
}

object LogicalInferenceSpec extends App {
  val k = 5
  val db = new TensorKB(k)
  db.sampleTensor(10, 10, 0, 0.1)
  db.toFactorGraph

  db += Cell("r6", "e6", DefaultIx, 0.0, CellType.Test)

  db += Impl("r4", "r6")
  db += Impl("r6", "r2")

  println(db.toVerboseString())

  LogicalInference(db, newCellType = CellType.Inferred)

  println(db.toVerboseString())

  println("Inferred cells:\n" + db.inferredCells.mkString("\n"))
}