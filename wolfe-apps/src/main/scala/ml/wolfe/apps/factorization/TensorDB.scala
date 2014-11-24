package ml.wolfe.apps.factorization

/**
 * @author: rockt
 */
object CellType extends Enumeration {
  type CellType = Value
  val Train, Dev, Test, Observed = Value
}

case object DefaultIx

import java.io.FileWriter

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
      keys1 += key1
      new ListBuffer[(Any, Any)]()
    }) append (key2 -> key3)
    if (key2 != DefaultIx) ix2Map.getOrElseUpdate(key2, {
      keys2 += key2
      new ListBuffer[(Any, Any)]()
    }) append (key1 -> key3)
    if (key3 != DefaultIx) {
      ix3Map.getOrElseUpdate(key3, {
        keys3 += key3
        new ListBuffer[(Any, Any)]()
      }) append (key1 -> key2)
      ix23Map.getOrElseUpdate(key2 -> key3, {
        keys23 += (key2 -> key3)
        new ListBuffer[Any]()
      }) append key1
    }

    cells append cell
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


  def writeVectors(filePath: String) = {
    val writer = new FileWriter(filePath)
    (ix1ToNodeMap ++ ix2ToNodeMap).foreach(p => {
      val (name, node) = p
      val vec = node.variable.asVector.b
      writer.write(name + "\t" + vec.mkString("\t") + "\n")
    })
    writer.close()
  }


  @tailrec
  final def sampleNodeFrom2(key1: CellIx, attempts: Int = 1000): Node = {
    if (isMatrix)
      if (attempts == 0) {
        println("WARNING: Could not find negative cell 2 for key1: " + key1.toString)
        ix2ToNodeMap(keys2(random.nextInt(keys2.size)))
      } else {
        val key2 = keys2(random.nextInt(keys2.size))
        if (get(key1, key2).exists(_.cellType == CellType.Train)) sampleNodeFrom2(key1, attempts - 1)
        else ix2ToNodeMap(key2)
      }
    else ???
  }

  @tailrec
  final def sampleNodeFrom1(key2: CellIx, attempts: Int = 1000): Node = {
    if (isMatrix)
      if (attempts == 0) {
        println("WARNING: Could not find negative cell 1 for key2: " + key2.toString)
        ix1ToNodeMap(keys1(random.nextInt(keys1.size)))
      } else {
        val key1 = keys1(random.nextInt(keys1.size))
        if (get(key1, key2).exists(_.cellType == CellType.Train)) sampleNodeFrom1(key2, attempts - 1)
        else ix1ToNodeMap(key1)
      }
    else ???
  }
}

trait Features extends TensorDB {
  private val featAlphabet1 = new mutable.HashMap[String, Int]()
  private val featAlphabet2 = new mutable.HashMap[String, Int]()
  private val featAlphabet3 = new mutable.HashMap[String, Int]()
  private val featNames1    = new ArrayBuffer[String]
  private val featNames2    = new ArrayBuffer[String]
  private val featNames3    = new ArrayBuffer[String]

  def fnames1: Seq[String] = featNames1
  def fnames2: Seq[String] = featNames2
  def fnames3: Seq[String] = featNames3

  private val feat1Map = new mutable.HashMap[CellIx, mutable.LinkedHashSet[Int]]()
  private val feat2Map = new mutable.HashMap[CellIx, mutable.LinkedHashSet[Int]]()
  private val feat3Map = new mutable.HashMap[CellIx, mutable.LinkedHashSet[Int]]()

  def featureIndex1(feat: String) = featureIndex(feat, featAlphabet1, featNames1)
  def featureIndex2(feat: String) = featureIndex(feat, featAlphabet2, featNames2)
  def featureIndex3(feat: String) = featureIndex(feat, featAlphabet3, featNames3)

  def numFeatures1 = featNames1.size
  def numFeatures2 = featNames2.size
  def numFeatures3 = featNames3.size

  private var _frozen  = false

  private var _fnode1: Option[Node] = None
  private var _fnode2: Option[Node] = None
  private var _fnode3: Option[Node] = None

  def fnode1 = _fnode1
  def fnode2 = _fnode2
  def fnode3 = _fnode3
  def fnodes = fnode1.toSeq ++ fnode2.toSeq ++ fnode3.toSeq

  def fweights1 = _fnode1.map(_.variable.asVector.b)
  def fweights2 = _fnode2.map(_.variable.asVector.b)
  def fweights3 = _fnode3.map(_.variable.asVector.b)

  def featureIndex(feat: String, alpha: mutable.HashMap[String, Int], names: ArrayBuffer[String]): Int = alpha.getOrElseUpdate(feat,
    if (_frozen) {
      sys.error(s"Cannot find feature $feat, alhpabet is frozen.")
      sys.exit(1)
    } else {
      val idx = names.size
      names += feat
      idx
    })

  def addFeat1(key1: CellIx, feat: String) = {
    val idx = featureIndex1(feat)
    feat1Map.getOrElseUpdate(key1, new mutable.LinkedHashSet) += idx
  }

  def addFeat2(key2: CellIx, feat: String) = {
    val idx = featureIndex2(feat)
    feat2Map.getOrElseUpdate(key2, new mutable.LinkedHashSet) += idx
  }

  def addFeat3(key3: CellIx, feat: String) = {
    val idx = featureIndex3(feat)
    feat3Map.getOrElseUpdate(key3, new mutable.LinkedHashSet) += idx
  }
  override def toFactorGraph: FactorGraph = {
    val fg = super.toFactorGraph
    _frozen = true
    if (isMatrix) {
      _fnode1 = Some(fg.addVectorNode(numFeatures1, "feats1"))
      _fnode2 = Some(fg.addVectorNode(numFeatures2, "feats2"))
      _fnode3 = Some(fg.addVectorNode(numFeatures3, "feats3"))
    } else ???
    fg
  }
}

class TensorKB(k: Int = 100) extends TensorDB(k) {
  def relations = keys1
  def arg1s = keys2
  def arg2s = keys3

  def getFact(relation: CellIx, entity1: CellIx, entity2: CellIx) = get(relation, entity1, entity2)
}

object LogicalInference {
  def apply(db: TensorDB, formulaList: List[Formula] = Nil, newCellType: CellType = CellType.Train): Unit = {
    var converged = false

    /*
    val formulae = if (formulaList.isEmpty) db.formulae.toList else formulaList
    while (!converged) {
      converged = true

      for (formula <- formulae) formula match {
        case Impl(p1, p2) =>
          val cs = db.getBy1(p1)
          cs.foreach(c => {
            val (c1, c2) = c
            val cellOpt = db.get(p2, c1, c2)

            if (!cellOpt.isDefined) {
              converged = false
              db += Cell(p2, c1, c2, target = 1.0, cellType = newCellType)
            }
          })
        case ImplNeg(p1, p2, _) =>
          val cs = db.getBy1(p1)
          cs.foreach(c => {
            val (c1, c2) = c
            val cellOpt = db.get(p2, c1, c2)

            if (!cellOpt.isDefined) {
              converged = false
              db += Cell(p2, c1, c2, target = 0.0, cellType = newCellType)
            }
          })
        case _ => ???
      }
    }
    */
  }
}