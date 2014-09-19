package ml.wolfe.apps

/**
 * Created by rockt on 19/09/2014.
 */
object CellType extends Enumeration {
  type CellType = Value
  val Train, Dev, Test, Observed = Value
}

import CellType._
import ml.wolfe.FactorGraph
import ml.wolfe.FactorGraph.Node

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

case class Cell(ix: Any, target: Double = 1.0, cellType: CellType = CellType.Train) {
  val train =     cellType == Train
  val dev =       cellType == Dev
  val test =      cellType == Test
  val observed =  cellType == Observed
}

class TensorDB(k: Int = 100) {
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

  type CellKey = (Any, Any, Any)
  case object EmptyIx

  /**
   * Cells can be indexed by `Any` data structure.
   */
  val cellMap = new mutable.HashMap[CellKey, Cell]()

  val ix1Map = new mutable.HashMap[Any, ListBuffer[(Any, Any)]]()
  val ix2Map = new mutable.HashMap[Any, ListBuffer[(Any, Any)]]()
  val ix3Map = new mutable.HashMap[Any, ListBuffer[(Any, Any)]]()
  val ix23Map = new mutable.HashMap[(Any, Any), ListBuffer[Any]]()

  val keys1 = new ArrayBuffer[Any]()
  val keys2 = new ArrayBuffer[Any]()
  val keys3 = new ArrayBuffer[Any]()
  val keys23 = new ArrayBuffer[Any]()

  def isMatrix = keys1.size > 0 && keys2.size > 0 && keys3.isEmpty
  def isTensor = keys3.nonEmpty

  val ix1ToNodeMap = new mutable.HashMap[Any, Node]()
  val ix2ToNodeMap = new mutable.HashMap[Any, Node]()
  val ix3ToNodeMap = new mutable.HashMap[Any, Node]()
  //for pair-embeddings
  val ix23ToNodeMap = new mutable.HashMap[(Any, Any), Node]()

  //this should be made simpler
  def get(key1: Any, key2: Any = EmptyIx, key3: Any = EmptyIx): Option[Cell] = key1 match {
    case (a, b) if key3 == EmptyIx => get(a, b)
    case (a, b, c) if key2 == EmptyIx && key3 == EmptyIx => get(a, b, c)
    case a if key2 != EmptyIx => key2 match {
      case (b, c) => get(a, b, c)
      case _ => cellMap.get((key1, key2, key3))
    }
    case _ => cellMap.get((key1, key2, key3))
  }

  def getBy1(key: Any) = ix1Map.getOrElse(key, List())
  def getBy2(key: Any) = ix2Map.getOrElse(key, List())
  def getBy3(key: Any) = ix3Map.getOrElse(key, List())

  def ++=(cells: Seq[Cell]) = cells foreach (this += _)

  def getKey(ix: Any, depth: Int = 1, key: CellKey = (EmptyIx, EmptyIx, EmptyIx)): CellKey = {
    def modifyKey(ix: Any, depth: Int, key: CellKey): CellKey = depth match {
      case 1 => (ix, key._2, key._3)
      case 2 => (key._1, ix, key._3)
      case 3 => (key._1, key._2, ix)
    }

    if (depth > 3) key
    else ix match {
      case Nil => key
      case x :: y :: zs if depth == 3 => modifyKey(x :: y :: zs, depth, key)
      case x :: ys => modifyKey(x, depth, getKey(ys, depth + 1))
      case (x, ys) if depth == 3 => modifyKey((x, ys), depth, key)
      case (x, ys) => modifyKey(x, depth, getKey(ys, depth + 1))
      case (x, y, zs) => modifyKey(x, depth, getKey((y, zs), depth + 1))
      case _ => modifyKey(ix, depth, key)
    }
  }

  def +=(cell: Cell) {
    val key = getKey(cell.ix, 1)

    cellMap += key -> cell

    val (key1, key2, key3) = key
    ix1Map.getOrElseUpdate(key1, {
      keys1 += key1
      new ListBuffer[(Any, Any)]()
    }) append ((key2, key3))
    if (key2 != EmptyIx) ix2Map.getOrElseUpdate(key2, {
      keys2 += key2
      new ListBuffer[(Any, Any)]()
    }) append ((key1, key3))
    if (key3 != EmptyIx) {
      ix3Map.getOrElseUpdate(key3, {
        keys3 += key3
        new ListBuffer[(Any, Any)]()
      }) append ((key1, key2))
      ix23Map.getOrElseUpdate((key2, key3), {
        keys23 += ((key2, key3))
        new ListBuffer[Any]()
      }) append key1
    }

    cells append cell
  }

  def toFactorGraph: FactorGraph = {
    val fg = new FactorGraph()

    if (isMatrix) {
      ix1ToNodeMap ++= keys1 map (key => key -> fg.addVectorNode(k))
      ix2ToNodeMap ++= keys2 map (key => key -> fg.addVectorNode(k))
    } else ???

    fg
  }

  private def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

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
        if (showTrain) sb ++= (if (get(col, row).isDefined) cellFormat.format("1").onGreen() else cellFormat.format(" "))
        else {
          val colVec = ix1ToNodeMap(col).variable.asVector.b
          val rowVec = ix2ToNodeMap(row).variable.asVector.b
          val p = sig(rowVec dot colVec)
          val pString = cellFormat.format(pFormat.format(p))

          sb ++= (
            if (get(col, row).map(_.target).getOrElse(0.0) >= 0.5)
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
    val rels = (1 to num1).map(i => s"r$i")
    val arg1s = (1 to num2).map(i => s"e$i")
    val arg2s = if (num3 > 0) (1 to num3).map(i => s"e$i") else List(EmptyIx)
    val rand = new Random(0l)
    for {
      r <- rels
      e1 <- arg1s
      e2 <- arg2s
      if e1 != e2
      if rand.nextDouble() <= density
    } {
      this += Cell(r -> (e1 -> e2))
    }
  }

  @tailrec
  final def sampleNode(col: Any, attempts: Int = 1000): Node = {
    if (isMatrix)
      if (attempts == 0) ix2ToNodeMap(keys2(random.nextInt(keys2.size)))
      else {
        val row = keys2(random.nextInt(keys2.size))
        if (get(col, row).isDefined) sampleNode(col, attempts - 1)
        else ix2ToNodeMap(row)
      }
    else ???
  }
}

class TensorKB(k: Int = 100) extends TensorDB(k) {
  def relations = keys1
  def arg1s = keys2
  def arg2s = keys3
 
  def getFact(relation: Any, entity1: Any, entity2: Any) = get(relation, entity1, entity2)
}