package ml.wolfe.apps.factorization

/**
 * @author rockt
 */
import java.io.File
import java.io.FileWriter

import ml.wolfe.util.Conf


object CellType extends Enumeration {
  type CellType = Value
  val Train, Dev, Test, Observed, Inferred = Value
}

import cc.factorie.la.DenseTensor1
import cc.factorie.la.SparseBinaryTensor1
import ml.wolfe.FactorGraph
import ml.wolfe.FactorGraph.Node
import ml.wolfe.apps.factorization.CellType._
import org.json4s.NoTypeHints
import org.json4s.native.Serialization

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.Random

case object DefaultIx

object TensorDBImplicits {
  implicit def stringToImpl(s: String): Formula2 = {
    val Array(premise, consequent) = s.split(" => ")
    consequent.head match {
      case '!' => ImplNeg(premise, consequent.tail)
      case _ => Impl(premise, consequent)
    }
  }

  implicit def tuple2ToCell(tuple2: (Any, Any)): Cell = Cell(tuple2._1, tuple2._2)
  implicit def tuple3ToCell(tuple3: (Any, Any, Any)): Cell = Cell(tuple3._1, tuple3._2, tuple3._3)
}

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
  def apply(key: Any)(implicit db: TensorDB): Double
}

abstract class Formula2(p1: Any, p2: Any) extends Formula {
  override val predicates: Seq[Any] = Seq(p1,p2)
}

case class Impl(p1: Any, p2: Any, target: Double = 1.0) extends Formula2(p1, p2) {
  override def apply(key: Any)(implicit db: TensorDB): Double = db.prob(p1, key) * (db.prob(p2, key) - 1) + 1
  override def toString: String = p1 + " => " + p2
}

case class ImplNeg(p1: Any, p2: Any, target: Double = 1.0) extends Formula2(p1, p2) {
  override def apply(key: Any)(implicit db: TensorDB): Double = db.prob(p1, key) * -db.prob(p2, key) + 1
  override def toString: String = p1 + " => !" + p2
}

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

  val trainCells =  new mutable.ListBuffer[Cell]()
  val devCells =  new mutable.ListBuffer[Cell]()
  val testCells =  new mutable.ListBuffer[Cell]()
  val observedCells =  new mutable.ListBuffer[Cell]()
  val inferredCells =  new mutable.ListBuffer[Cell]()

  //for efficient checking whether a test cell has an inferred value
  val inferredCellsMap = new mutable.HashMap[(CellIx, CellIx), Cell]()

  lazy val testIx1 = testCells.map(_.key1).toSet
  lazy val testIx2 = testCells.map(_.key2).toSet
  lazy val testIx3 = testCells.map(_.key3).toSet

  /**
   * @return number of cells in the tensor
   */
  def numCells = cells.size //fixme: overcounts if you have multiple cells (e.g. test and inferred) for the same key


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

  def getPredictedBy1(key: CellIx, threshold: Double = 0.5) = keys2.filter(this.prob(key, _) >= threshold).map((_, DefaultIx))
  def getPredictedBy2(key: CellIx, threshold: Double = 0.5) = keys1.filter(this.prob(key, _) >= threshold).map((_, DefaultIx))
  //fixme: currently only applicable to matrices
  def getPredictedBy3(key: CellIx, threshold: Double = 0.5) = ???
  def getPredictedBy23(key1: CellIx, key2: CellIx, threshold: Double = 0.5) = ???

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

    cell.cellType match {
      case Train => trainCells append cell
      case Dev => devCells append cell
      case Test => testCells append cell
      case Observed => observedCells append cell
      case Inferred =>
        inferredCells append cell
        inferredCellsMap += (key1, key2) -> cell
    }
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


  @deprecated
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
  final def sampleNodeFrom2(key1: CellIx, attempts: Int = 1000, sampleTestRows: Boolean = true): Node = {
    if (isMatrix)
      if (attempts == 0) {
        println("WARNING: Could not find negative cell 2 for key1: " + key1.toString)
        ix2ToNodeMap(keys2(random.nextInt(keys2.size)))
      } else {
        val key2 = keys2(random.nextInt(keys2.size))
        if (get(key1, key2).exists(_.cellType == CellType.Train) || (!sampleTestRows && testIx2(key2)))
          sampleNodeFrom2(key1, attempts - 1)
        else
          ix2ToNodeMap(key2)
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
        if (get(key1, key2).exists(_.cellType == CellType.Train))
          sampleNodeFrom1(key2, attempts - 1)
        else
          ix1ToNodeMap(key1)
      }
    else ???
  }

  /**
   * Only works if your tensor is indexed by Strings
   * @param pathToDir path to directory where the serialized tensor will be stored
   */
  def serialize(pathToDir: String) = {
    implicit val formats = Serialization.formats(NoTypeHints)

    val dir = new File(pathToDir)
    dir.mkdirs()

    val cellWriter = new FileWriter(dir.getAbsolutePath + "/cells.txt")
    //cells.foreach(c => cellWriter.write(Serialization.write(c) + "\n"))
    cells.foreach(c => cellWriter.write(s"${c.key1}\t${c.key2}\t${c.key3}\t${c.target}\t${c.cellType}\n"))
    cellWriter.close()

    def writeVectors(ixToNodeMap: mutable.HashMap[CellIx, Node], writer: FileWriter): Unit = {
      ixToNodeMap.foreach(t => {
        val (ix, node) = t
        val vec = node.variable.asVector.b.toArray
        writer.write(ix + "\t" + Serialization.write(vec) + "\n")
      })
      writer.close()
    }

    writeVectors(ix1ToNodeMap, new FileWriter(dir.getAbsolutePath + "/rows.txt"))
    writeVectors(ix2ToNodeMap, new FileWriter(dir.getAbsolutePath + "/cols.txt"))
  }

  def deserialize(pathToDir: String) = {
    implicit val formats = Serialization.formats(NoTypeHints)

    val dir = new File(pathToDir)
    //Source.fromFile(dir.getAbsolutePath + "/cells.txt").getLines().foreach(l => Serialization.read[Cell](l))
    Source.fromFile(dir.getAbsolutePath + "/cells.txt").getLines().foreach(l => {
      val Array(key1String, key2String, key3String, targetString, cellTypeString) = l.split("\t")

      val key1 = if (key1String == "DefaultIx") DefaultIx else key1String
      val key2 = if (key2String == "DefaultIx") DefaultIx else key2String
      val key3 = if (key3String == "DefaultIx") DefaultIx else key3String

      val target = targetString.toDouble

      val cellType = cellTypeString match {
        case "Train" => CellType.Train
        case "Dev" => CellType.Dev
        case "Test" => CellType.Test
        case "Observed" => CellType.Observed
        case "Inferred" => CellType.Inferred
      }

      this += Cell(key1, key2, key3, target, cellType)
    })

    val fg = toFactorGraph
    fg.build()

    Source.fromFile(dir.getAbsolutePath + "/rows.txt").getLines().foreach(l => {
      val Array(name, vec) = l.split("\t")
      ix1ToNodeMap(name).variable.asVector.b = new DenseTensor1(Serialization.read[Array[Double]](vec))
    })

    Source.fromFile(dir.getAbsolutePath + "/cols.txt").getLines().foreach(l => {
      val Array(name, vec) = l.split("\t")
      ix2ToNodeMap(name).variable.asVector.b = new DenseTensor1(Serialization.read[Array[Double]](vec))
    })
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

  def features1(key1: CellIx) = feat1Map.getOrElse(key1, Set.empty)
  def features2(key2: CellIx) = feat2Map.getOrElse(key2, Set.empty)
  def features3(key3: CellIx) = feat3Map.getOrElse(key3, Set.empty)

  def numFeatures1 = featNames1.size
  def numFeatures2 = featNames2.size
  def numFeatures3 = featNames3.size

  private var _frozen  = false

  private val _fwnode1s: mutable.HashMap[CellIx, Node] = new mutable.HashMap()
  private val _fwnode2s: mutable.HashMap[CellIx, Node] = new mutable.HashMap()

  def fwnode1(key2: CellIx) = _fwnode1s.get(key2)
  def fwnodes1 = _fwnode1s.values
  def fwnode2(key1: CellIx) = _fwnode2s.get(key1)
  def fwnodes2 = _fwnode2s.values

  private val _fnode1s: mutable.HashMap[CellIx, Node] = new mutable.HashMap()
  private val _fnode2s: mutable.HashMap[CellIx, Node] = new mutable.HashMap()

  def fnode1(key1: CellIx) = _fnode1s.get(key1)
  def fnodes1 = _fnode1s.values
  def fnode2(key2: CellIx) = _fnode2s.get(key2)
  def fnodes2 = _fnode2s.values

  def featureIndex(feat: String, alpha: mutable.HashMap[String, Int], names: ArrayBuffer[String]): Int = alpha.getOrElseUpdate(feat,
    if (_frozen) {
      sys.error(s"Cannot find feature $feat, alphabet is frozen.")
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
      if(numFeatures1 > 0) {
        for(key1 <- keys1) {
          val feats = features1(key1)
          val fvector = new SparseBinaryTensor1(numFeatures1)
          fvector ++= feats
          val fnode = fg.addVectorNode(numFeatures1, "f1" + ":" + key1.toString)
          fnode.variable.asVector.b = fvector
          _fnode1s(key1) = fnode
        }
        for(key2 <- keys2) _fwnode1s(key2) = fg.addVectorNode(numFeatures1, "fw1" + ":" + key2.toString)
        println(s"Feature 1 nodes added, ${_fnode1s.size} fnodes and ${_fwnode1s} weight nodes.")
      }
      if(numFeatures2 > 0) {
        for(key2 <- keys2) {
          val feats = features2(key2)
          val fvector = new SparseBinaryTensor1(numFeatures2)
          fvector ++= feats
          val fnode = fg.addVectorNode(numFeatures2, "f2" + ":" + key2.toString)
          fnode.variable.asVector.b = fvector
          _fnode2s(key2) = fnode
        }
        for(key1 <- keys1) _fwnode2s(key1) = fg.addVectorNode(numFeatures2, "fw2" + ":" + key1.toString)
        println(s"Feature 2 nodes added, ${_fnode2s.size} fnodes and ${_fwnode2s.size} weight nodes.")
      }
    } else ???
    fg
  }

  private def writeFeatureVector(filePath:String, names: Seq[String], nodes: Iterable[Node]): Unit = {
    // names
    val namesWriter = new FileWriter(filePath + ".names")
    for(n <- names) namesWriter.write(n +"\n")
    namesWriter.flush()
    namesWriter.close()
    // vectors
    val writer = new FileWriter(filePath + ".vecs")
    for(n <- nodes) {
      val vec = n.variable.asVector.b
      val name = n.variable.label
      writer.write(name + "\t" + vec.mkString("\t") + "\n")
    }
    writer.flush()
    writer.close()
  }

  override def writeVectors(filePath: String): Unit = {
    super.writeVectors(filePath)
    if (numFeatures1 > 0) {
      writeFeatureVector(filePath + ".feats1", fnames1, fwnodes1)
    }
    if (numFeatures2 > 0) {
      writeFeatureVector(filePath + ".feats2", fnames2, fwnodes2)
    }
  }
}

class TensorKB(k: Int = 100) extends TensorDB(k) {
  def relations = keys1
  def arg1s = keys2
  def arg2s = keys3

  def getFact(relation: CellIx, entity1: CellIx, entity2: CellIx) = get(relation, entity1, entity2)
}