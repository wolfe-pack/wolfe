package ml.wolfe.apps.factorization

/**
 * @author rockt
 */
object CellType extends Enumeration {
  type CellType = Value
  val Train, Dev, Test, Observed = Value
}

case object DefaultIx

import ml.wolfe.FactorGraph
import ml.wolfe.FactorGraph.Node
import ml.wolfe.apps.factorization.CellType._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

case class Cell(key1: Any, key2: Any = DefaultIx, key3: Any = DefaultIx, target: Double = 1.0, cellType: CellType = CellType.Train) {
  val key = (key1, key2, key3)
  val train =    cellType == Train
  val dev =      cellType == Dev
  val test =     cellType == Test
  val observed = cellType == Observed
}

trait Formula {
  val predicates: Seq[Any]
  def isFormula2 = predicates.size == 2
}

abstract class Formula2(p1: Any, p2: Any) extends Formula {
  override val predicates: Seq[Any] = Seq(p1,p2)
}

case class Impl(p1: Any, p2: Any, target: Double = 1.0) extends Formula2(p1, p2)
case class ImplNeg(p1: Any, p2: Any, target: Double = 1.0) extends Formula2(p1, p2)



trait Tensor {
  type CellIx = Any
  type CellKey = (CellIx, CellIx, CellIx)

  def get(key1: CellIx, key2: CellIx = DefaultIx, key3: CellIx = DefaultIx): Option[Cell]
}

class TensorDB(k: Int = 100) extends Tensor {
  val random = new Random(0l)

  /**
   * Represents training, dev and test cells in a sparse tensor.
   */
  val cells = new mutable.ListBuffer[Cell]()

  def trainCells =    cells.filter(_.train)
  def devCells =      cells.filter(_.dev)
  def testCells =     cells.filter(_.test)
  def observedCells = cells.filter(_.observed)

  /**
   * @return number of cells in the tensor
   */
  def numCells = cells.size


  /**
   * Cells can be indexed by `Any` data structure.
   */
  val cellMap = new mutable.HashMap[CellKey, Cell]()

  val ix1Map = new mutable.HashMap[CellIx, ListBuffer[(CellIx, CellIx)]]()
  val ix2Map = new mutable.HashMap[CellIx, ListBuffer[(CellIx, CellIx)]]()
  val ix3Map = new mutable.HashMap[CellIx, ListBuffer[(CellIx, CellIx)]]()
  val ix23Map = new mutable.HashMap[(CellIx, CellIx), ListBuffer[CellIx]]()

  val keys1 = new ArrayBuffer[CellIx]()
  val keys2 = new ArrayBuffer[CellIx]()
  val keys3 = new ArrayBuffer[CellIx]()
  val keys23 = new ArrayBuffer[CellIx]()

  val cellIxToIntIx1 = new mutable.HashMap[CellIx, Int]()
  val cellIxToIntIx2 = new mutable.HashMap[CellIx, Int]()
  val cellIxToIntIx3 = new mutable.HashMap[CellIx, Int]()
  val cellIxToIntIx23 = new mutable.HashMap[(CellIx, CellIx), Int]()

  def dim1 = keys1.size
  def dim2 = keys2.size
  def dim3 = keys3.size

  def isEmpty = cells.isEmpty
  def isMatrix = keys1.size > 0 && keys2.size > 0 && keys3.isEmpty
  def isTensor = keys3.nonEmpty

  val ix1ToNodeMap = new mutable.HashMap[CellIx, Node]()
  val ix2ToNodeMap = new mutable.HashMap[CellIx, Node]()
  val ix3ToNodeMap = new mutable.HashMap[CellIx, Node]()
  //for pair-embeddings
  val ix23ToNodeMap = new mutable.HashMap[(CellIx, CellIx), Node]()

  def get(key1: CellIx, key2: CellIx = DefaultIx, key3: CellIx = DefaultIx): Option[Cell] =
    cellMap.get((key1, key2, key3))

  def getBy1(key: CellIx) = ix1Map.getOrElse(key, List())
  def getBy2(key: CellIx) = ix2Map.getOrElse(key, List())
  def getBy3(key: CellIx) = ix3Map.getOrElse(key, List())
  def getBy23(key1: CellIx, key2: CellIx) = ix23Map.getOrElse(key1 -> key2, List())

  def ++=(cells: Seq[Cell]) = cells foreach (this += _)

  def node1(key1: CellIx) = ix1ToNodeMap.get(key1)
  def node2(key2: CellIx) = ix2ToNodeMap.get(key2)
  def node3(key3: CellIx) = ix3ToNodeMap.get(key3)
  def node23(key23: (CellIx, CellIx)) = ix23ToNodeMap.get(key23)

  def vector1(key1: CellIx) = node1(key1).map(_.variable.asVector.b)
  def vector2(key2: CellIx) = node2(key2).map(_.variable.asVector.b)
  def vector3(key3: CellIx) = node3(key3).map(_.variable.asVector.b)
  def vector23(key23: (CellIx, CellIx)) = node23(key23).map(_.variable.asVector.b)

  def +=(cell: Cell) {
    cellMap += cell.key -> cell

    val (key1, key2, key3) = cell.key
    ix1Map.getOrElseUpdate(key1, {
      cellIxToIntIx1 += key1 -> keys1.size
      keys1 += key1
      new ListBuffer[(Any, Any)]()
    }) append (key2 -> key3)
    if (key2 != DefaultIx) ix2Map.getOrElseUpdate(key2, {
      cellIxToIntIx2 += key2 -> keys2.size
      keys2 += key2
      new ListBuffer[(Any, Any)]()
    }) append (key1 -> key3)
    if (key3 != DefaultIx) {
      ix3Map.getOrElseUpdate(key3, {
        cellIxToIntIx3 += key3 -> keys3.size
        keys3 += key3
        new ListBuffer[(Any, Any)]()
      }) append (key1 -> key2)
      ix23Map.getOrElseUpdate(key2 -> key3, {
        cellIxToIntIx23 += (key2, key3) -> keys23.size
        keys23 += (key2 -> key3)
        new ListBuffer[Any]()
      }) append key1
    }

    cells append cell
  }

  def -=(cell: Cell) = {
    cells -= cell
    keys1 -= cell
    keys2 -= cell
    keys3 -= cell
    keys23 -= cell

    cellMap.find(_._2 == cell).map(t => cellMap.remove(t._1))
  }

  val formulae = new ArrayBuffer[Formula]()

  def +=(formula: Formula) = formulae += formula

  def formulaeByPredicate(predicate: CellIx): Seq[Formula] = formulae.filter(_.predicates.contains(predicate))


  def toFactorGraph: FactorGraph = {
    val fg = new FactorGraph()

    if (isMatrix) {
      ix1ToNodeMap ++= keys1 map (key => key -> fg.addVectorNode(k, key.toString))
      ix2ToNodeMap ++= keys2 map (key => key -> fg.addVectorNode(k, key.toString))
    } else ???

    fg
  }

  private def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  def prob(key1: CellIx, key2: CellIx): Double = sig(vector1(key1).get dot vector2(key2).get)

  def toVerboseString(showTrain: Boolean = false) = {
    import ml.wolfe.nlp.util.ANSIFormatter._

    val cols = keys1
    val rows = if (isMatrix) keys2 else keys23

    val colWidth = math.max(keys1.map(_.toString.length).max + 1, 5)
    val firstColWidth = rows.map(_.toString.length).max + 1

    val colFormat = "%"+colWidth+"s"
    val firstColFormat = "%"+firstColWidth+"s"
    val cellFormat = "%"+(colWidth-1)+"s "
    val pFormat = "%4.2f"

    val sb = new mutable.StringBuilder()
    sb ++= " " * firstColWidth
    cols.foreach(col => sb ++= colFormat.format(col))
    sb ++= "\n"
    rows.foreach(row => {
      sb ++= firstColFormat.format(row) + " "
      cols.foreach(col => {
        val cellOpt =
          if (isMatrix) get(col, row)
          else {
            val (key2, key3) = row
            get(col, key2, key3)
          }


        if (showTrain) {
          sb ++= (if (cellOpt.isDefined) cellFormat.format("1").onGreen() else cellFormat.format(" "))
        }

        else {
          val colVec = ix1ToNodeMap(col).variable.asVector.b
          val rowVec = ix2ToNodeMap(row).variable.asVector.b
          val p = sig(rowVec dot colVec)
          val pString = cellFormat.format(pFormat.format(p))

          sb ++= (
            if (cellOpt.map(_.target).getOrElse(0.0) >= 0.5)
              if (p >= 0.8) pString.onGreen()
              else if (p >= 0.5) pString.onYellow()
              else pString.onRed()
            else if (p >= 0.8) pString.red()
            else if (p >= 0.5) pString.yellow()
            else pString
          )
        }
      })
      sb ++= "\n"
    })
    sb.toString()
  }

  def toIndexString = cellMap.mkString("\n") + s"\n---\n$ix1Map\n$ix2Map\n$ix3Map\n"

  def sampleTensor(num1: Int, num2: Int, num3: Int = 0, density: Double = 0.1) = {
    require(cells.isEmpty)

    val rels = (1 to num1).map(i => s"r$i")
    val arg1s = (1 to num2).map(i => s"e$i")
    val arg2s = if (num3 > 0) (1 to num3).map(i => s"e$i") else List(DefaultIx)
    val rand = new Random(0l)
    for {
      r <- rels
      e1 <- arg1s
      e2 <- arg2s
      if e1 != e2
      if rand.nextDouble() <= density
    } {
      this += Cell(r, e1, e2)
    }
  }

  def toInfoString =
    s"""
      |#cols:     ${keys1.size}
      |#rows:     ${keys2.size}
      |#layers:   ${keys3.size}
      |#cells:    $numCells (${numCells.toDouble / (keys1.size * keys2.size * (if (keys3.size > 0) keys3.size else 1))})
      |#train:    ${trainCells.size} (${trainCells.size.toDouble / (keys1.size * keys2.size * (if (keys3.size > 0) keys3.size else 1))})
      |#dev:      ${devCells.size} (${devCells.size.toDouble / (keys1.size * keys2.size * (if (keys3.size > 0) keys3.size else 1))})
      |#test:     ${testCells.size} (${testCells.size.toDouble / (keys1.size * keys2.size * (if (keys3.size > 0) keys3.size else 1))})
      |#formulae: ${formulae.size}
    """.stripMargin

  @tailrec
  final def sampleNode(col: CellIx, attempts: Int = 1000): Node = {
    if (isMatrix)
      if (attempts == 0) ix2ToNodeMap(keys2(random.nextInt(keys2.size)))
      else {
        val row = keys2(random.nextInt(keys2.size))
        if (get(col, row).exists(_.cellType == CellType.Train)) sampleNode(col, attempts - 1)
        else ix2ToNodeMap(row)
      }
    else ???
  }
}

class TensorKB(k: Int = 100) extends TensorDB(k) {
  def relations = keys1
  def arg1s = keys2
  def arg2s = keys3
 
  def getFact(relation: CellIx, entity1: CellIx, entity2: CellIx) = get(relation, entity1, entity2)
}