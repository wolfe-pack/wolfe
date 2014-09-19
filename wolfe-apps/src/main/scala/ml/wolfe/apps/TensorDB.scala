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

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

case class Cell(ix: Any, target: Double = 1.0, cellType: CellType = CellType.Train) {
  val train =     cellType == Train
  val dev =       cellType == Dev
  val test =      cellType == Test
  val observed =  cellType == Observed
}

class TensorDB(k: Int = 100) {
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

  def keys1 = ix1Map.keySet
  def keys2 = ix2Map.keySet
  def keys3 = ix3Map.keySet
  def keys23 = ix23Map.keySet

  def isMatrix = keys1.size != 0 && keys2.size != 0 && keys3.isEmpty
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
    ix1Map.getOrElseUpdate(key1, new ListBuffer[(Any, Any)]()) append ((key2, key3))
    if (key2 != EmptyIx) ix2Map.getOrElseUpdate(key2, new ListBuffer[(Any, Any)]()) append ((key1, key3))
    if (key3 != EmptyIx) {
      ix3Map.getOrElseUpdate(key3, new ListBuffer[(Any, Any)]()) append ((key1, key2))
      ix23Map.getOrElseUpdate((key2, key3), new ListBuffer[Any]()) append key1
    }

    cells append cell
  }

  def toFactorGraph: FactorGraph = ???

  def toVerboseString = {
    import ml.wolfe.nlp.util.ANSIFormatter._
    val w1 = keys1.map(_.toString.length).max + 1
    val rows = if (isMatrix) keys2 else keys23
    val w2 = rows.map(_.toString.length).max + 1

    val format1 = "%"+w1+"s"
    val format2 = "%"+w2+"s"
    val format3 = "%"+(w1-1)+"s "

    val sb = new mutable.StringBuilder()
    sb ++= " " * w2
    ix1Map.keySet.foreach(col => sb ++= format1.format(col))
    sb ++= "\n"
    rows.foreach(row => {
      sb ++= format2.format(row)
      ix1Map.keySet.foreach(col => {
        sb ++= (if (get(col, row).isDefined) format3.format("1").onGreen() else format3.format(" "))
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
      e1 <- arg1s
      e2 <- arg2s
      if e1 != e2
      r <- rels
      if rand.nextDouble() <= density
    } {
      this += Cell(r -> (e1 -> e2))
    }
  }
}

class TensorKB(k: Int = 100) extends TensorDB(k) {
  def relations = keys1
  def arg1s = keys2
  def arg2s = keys3
 
  def getFact(relation: Any, entity1: Any, entity2: Any) = get(relation, entity1, entity2)
}