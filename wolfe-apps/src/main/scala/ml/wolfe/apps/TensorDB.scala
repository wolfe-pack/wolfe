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

  val ix1Map = new mutable.HashMap[Any, mutable.ListBuffer[Cell]]()
  val ix2Map = new mutable.HashMap[Any, mutable.ListBuffer[Cell]]()
  val ix3Map = new mutable.HashMap[Any, mutable.ListBuffer[Cell]]()

  val ix1ToNodeMap = new mutable.HashMap[Any, Node]()
  val ix2ToNodeMap = new mutable.HashMap[Any, Node]()
  val ix3ToNodeMap = new mutable.HashMap[Any, Node]()
  
  //for pair-embeddings
  val ix23ToNodeMap = new mutable.HashMap[(Any, Any), Node]()

  def get(key1: Any, key2: Any = EmptyIx, key3: Any = EmptyIx): Option[Cell] = key1 match {
      case (a, b) if key3 == EmptyIx => get(a, b)
      case (a, b, c) if key2 == EmptyIx && key3 == EmptyIx => get(a, b, c)
      case _ => cellMap.get((key1, key2, key3))
    }

  def get1(key: Any) = ix1Map.getOrElse(key, List())
  def get2(key: Any) = ix2Map.getOrElse(key, List())
  def get3(key: Any) = ix3Map.getOrElse(key, List())

  def ++=(cells: Seq[Cell]) = cells foreach (this += _)

  def modifyKey(ix: Any, depth: Int, key: CellKey): CellKey = depth match {
    case 1 => (ix, key._2, key._3)
    case 2 => (key._1, ix, key._3)
    case 3 => (key._1, key._2, ix)
  }

  def getKey(ix: Any, depth: Int = 1, key: CellKey = (EmptyIx, EmptyIx, EmptyIx)): CellKey =
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

  def +=(cell: Cell) {
    val key = getKey(cell.ix, 1)

    cellMap += key -> cell

    val (key1, key2, key3) = key
    ix1Map.getOrElseUpdate(key1, new ListBuffer[Cell]()) append cell
    if (key2 != EmptyIx) ix2Map.getOrElseUpdate(key2, new ListBuffer[Cell]()) append cell
    if (key3 != EmptyIx) ix3Map.getOrElseUpdate(key3, new ListBuffer[Cell]()) append cell

    cells append cell
  }

  def isMatrix = ix1Map.size != 0 && ix2Map.size != 0 && ix3Map.keySet == Set()

  def toFactorGraph: FactorGraph = ???

  def toStringVerbose = {
    val sb = new mutable.StringBuilder()
    sb ++= "\t"
    ix1Map.keySet.foreach(col => sb ++= s"$col\t")
    sb ++= "\n"
    ix2Map.keySet.foreach(row => {
      sb ++= s"$row\t"
      ix1Map.keySet.foreach(col => {
        sb ++= (if (get(Seq(col, row)).isDefined) "1" else " ")
      })
      sb ++= "\n"
    })
    sb.toString()
  }

  def toIxString = cellMap.mkString("\n") + s"\n---\n$ix1Map\n$ix2Map\n$ix3Map\n"

}