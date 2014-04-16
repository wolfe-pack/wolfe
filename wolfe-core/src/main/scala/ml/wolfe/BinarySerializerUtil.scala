package ml.wolfe

import java.io._
import cc.factorie.la._
import scala.collection.mutable
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

/**
* Created by larysa
*/

//todo: add Factorie header
// todo: feature vector (de)serialization is adapted from cc.factorie.util.BinarySerializer
object BinarySerializerUtil {


  private val INT: Byte = 0x01
  private val DOUBLE: Byte = 0x02
  private val BOOLEAN: Byte = 0x03
  private val STRING: Byte = 0x04
  private val TENSOR: Byte = 0x05
  private val MAP: Byte = 0x06
  private val LIST: Byte = 0x07
  private val NULL: Byte = 0x08
  private val SPARSE_INDEXED_TENSOR: Byte = 0x09
  private val SPARSE_BINARY_TENSOR: Byte = 0x10
  private val DENSE_TENSOR: Byte = 0x11

  private def tagForType(value: Any): Byte = value match {
    case _: Int => INT
    case _: Double => DOUBLE
    case _: Boolean => BOOLEAN
    case _: String => STRING
    case _: SparseIndexedTensor => SPARSE_INDEXED_TENSOR
    case _: SparseBinaryTensor => SPARSE_BINARY_TENSOR
    case _: DenseTensor => DENSE_TENSOR
    case _: Tensor => TENSOR
    case _: mutable.Map[String@unchecked, Any@unchecked] => MAP
    case _: Traversable[_] => LIST
    case null => NULL
  }

  private def isPrimitiveTag(tag: Byte): Boolean = tag match {
    case DOUBLE | BOOLEAN | INT | NULL => true
    case _ => false
  }

  private def isPrimitive(value: Any): Boolean = isPrimitiveTag(tagForType(value))
  def serializeFeatureVector(t: Tensor, file: String, gzip: Boolean = false) = {
    serialize(t, new File(file), gzip)
  }

  def deserializeFeatureVector(file: String, gzip: Boolean = false): Any = {
    val stream: DataInputStream = readFile(new File(file), gzip)
    val vector: Any = deserialize(stream)
    stream.close()
    vector
  }

  private def deserialize(s: DataInputStream): Any = {
    deserialize(s.readByte(), s)
  }

  private def deserialize(tag: Byte, s: DataInputStream): Any = tag match {
    case DOUBLE => s.readDouble()
    case INT => s.readInt()
    case BOOLEAN => s.readShort() != 0
    case STRING => readString(s)
    case SPARSE_INDEXED_TENSOR | SPARSE_BINARY_TENSOR | DENSE_TENSOR =>
      val activeDomainSize = s.readInt()
      val dims = readIntArray(s)
      val order = dims.length
      val newBlank =
        (tag, order) match {
          case (SPARSE_INDEXED_TENSOR, 1) => new SparseIndexedTensor1(dims(0))
          case (SPARSE_INDEXED_TENSOR, 2) => new SparseIndexedTensor2(dims(0), dims(1))
          case (SPARSE_INDEXED_TENSOR, 3) => new SparseIndexedTensor3(dims(0), dims(1), dims(2))
          case (SPARSE_BINARY_TENSOR, 1) => new SparseBinaryTensor1(dims(0))
          case (SPARSE_BINARY_TENSOR, 2) => new SparseBinaryTensor2(dims(0), dims(1))
          case (SPARSE_BINARY_TENSOR, 3) => new SparseBinaryTensor3(dims(0), dims(1), dims(2))
          case (DENSE_TENSOR, 1) => new DenseTensor1(dims(0))
          case (DENSE_TENSOR, 2) => new DenseTensor2(dims(0), dims(1))
          case (DENSE_TENSOR, 3) => new DenseTensor3(dims(0), dims(1), dims(2))
        }
      newBlank match {
        case nb: ArraySparseIndexedTensor =>
          nb.sizeHint(activeDomainSize)
          val idxArr = readIntArray(s)
          val valArr = readDoubleArray(s)
          for ((i, v) <- idxArr.zip(valArr)) nb +=(i, v)
        case nb: ArraySparseBinaryTensor =>
          nb.sizeHint(activeDomainSize)
          nb ++= readIntArray(s)
        case nb: DenseTensor =>
          readDoubleArray(s, nb.asArray)
      }
      newBlank
    case MAP =>
      val m = new mutable.HashMap[String, Any]
      repeat(s.readInt()) {
        val key = readString(s)
        m(key) = deserialize(s.readByte(), s)
      }
      m
    case LIST =>
      val innerTag = s.readByte()
      val len = s.readInt()
      val buff =
        (if (innerTag == INT) new mutable.ArrayBuffer[Int]
        else if (innerTag == DOUBLE) new mutable.ArrayBuffer[Double]
        else new mutable.ArrayBuffer[Any]).asInstanceOf[mutable.ArrayBuffer[Any]]
      repeat(len) {
        val nextTag = if (isPrimitiveTag(innerTag)) innerTag else s.readByte() // read and ignore the type tag
        buff += deserialize(nextTag, s)
      }
      buff
    case NULL =>
      s.readByte()
      null
  }

  private def serializeFeatureVector(value: Any, s: DataOutputStream): Unit = {
    if (!isPrimitive(value)) s.writeByte(tagForType(value))
    value match {
      case i: Int => s.writeInt(i)
      case bl: Boolean => s.writeShort(if (bl) 0x01 else 0x00)
      case d: Double => s.writeDouble(d)
      case str: String => writeString(str, s)
      case t: Tensor if t.isInstanceOf[SparseIndexedTensor] || t.isInstanceOf[SparseBinaryTensor] || t.isInstanceOf[DenseTensor] =>
        val activeDomainSize = t.activeDomainSize
        s.writeInt(activeDomainSize)
        writeIntArray(s, t.dimensions)
        t match {
          case nb: SparseIndexedTensor =>
            writeIntArray(s, nb._indices, activeDomainSize)
            writeDoubleArray(s, nb._values, activeDomainSize)
          case nb: SparseBinaryTensor =>
            writeIntArray(s, nb._indices, activeDomainSize)
          case nb: DenseTensor => writeDoubleArray(s, nb.asArray, activeDomainSize)
        }
      case t: Tensor =>
        s.writeInt(t.activeDomainSize)
        t.foreachActiveElement((i, v) => {s.writeInt(i); s.writeDouble(v)})
      case m: mutable.Map[String@unchecked, Any@unchecked] =>
        s.writeInt(m.size)
        for ((k, v) <- m) serializeFeatureVector(v, s)
      case t: Traversable[Any@unchecked] =>
        val tag = t.headOption.map(tagForType).getOrElse(INT)
        s.writeByte(tag)
        s.writeInt(t.size)
        t.foreach(serializeFeatureVector(_, s))
      case null =>
        s.writeByte(0x0)
    }
    s.flush()
  }
  private def serialize(c: Tensor, file: File, gzip: Boolean = false): Unit = {
    val stream = writeFile(file, gzip)
    serialize(c, stream)
    stream.close()
  }
  private def serialize(c: Tensor, s: DataOutputStream): Unit = {
    serializeFeatureVector(c, s)
  }
  def writeFile(file: File, gzip: Boolean = false): DataOutputStream = {
    file.createNewFile()
    val fileStream = new BufferedOutputStream(new FileOutputStream(file))
    new DataOutputStream(if (gzip) new BufferedOutputStream(new GZIPOutputStream(fileStream)) else fileStream)
  }


  def readFile(file: File, gzip: Boolean = false): DataInputStream = {
    val fileStream = new BufferedInputStream(new FileInputStream(file))
    new DataInputStream(if (gzip) new BufferedInputStream(new GZIPInputStream(fileStream)) else fileStream)
  }
  private def writeString(str: String, s: DataOutputStream): Unit = {
    s.writeInt(str.length)
    str.foreach(s.writeChar(_))
  }
  private def readDoubleArray(s: DataInputStream, arr: Array[Double]): Array[Double] = { val length = s.readInt(); var i = 0; while (i < length) {arr(i) = s.readDouble(); i += 1}; arr }
  private def readDoubleArray(s: DataInputStream): Array[Double] = { val arr = new Array[Double](s.readInt()); var i = 0; while (i < arr.length) {arr(i) = s.readDouble(); i += 1}; arr }
  private def writeDoubleArray(s: DataOutputStream, arr: Array[Double], length: Int): Unit = { s.writeInt(length); var i = 0; while (i < length) {s.writeDouble(arr(i)); i += 1} }
  private def readIntArray(s: DataInputStream): Array[Int] = { val arr = new Array[Int](s.readInt()); var i = 0; while (i < arr.length) {arr(i) = s.readInt(); i += 1}; arr }
  private def writeIntArray(s: DataOutputStream, arr: Array[Int]): Unit = writeIntArray(s, arr, arr.length)
  private def writeIntArray(s: DataOutputStream, arr: Array[Int], length: Int): Unit = { s.writeInt(length); var i = 0; while (i < length) {s.writeInt(arr(i)); i += 1} }
  private def readString(s: DataInputStream): String = {
    val bldr = new StringBuilder
    repeat(s.readInt())(bldr += s.readChar())
    bldr.mkString
  }
  def repeat(n: Int)(f: => Unit): Unit = for (i <- 0 until n) f


  /* Index Serialization*/

  def serializeIndex(index: SimpleIndex, file: String) = {
    val stream = new ObjectOutputStream(new FileOutputStream(new File(file)))
    index.serialize(stream)
    stream.close()
  }

  def deserializeIndex(file: String): Index = {
    val stream = new ObjectInputStream(new FileInputStream(new File(file)))
    new SimpleIndex().deserialize(stream)
  }


}

object JsonSerializer {
  println(" = ")
}

//
//object JsonSerializer {
//
//  import java.io.{InputStream, File}
//  import scala.io.{ Source}
//  import scala.pickling.io.TextFileOutput
//  import scala.pickling._
//  import json._
//
//  def serialize[T: SPickler : FastTypeTag](t: T) = t.pickle
//
//  def serializeToFile[T: SPickler : FastTypeTag](t: T, filePath: String) = {
//    val tempFile: File = new File(filePath)
//    val file: TextFileOutput = new TextFileOutput(tempFile)
//    t.pickleTo(file)
//    file.close()
//  }
//
//  def deserializeFromFile[T: Unpickler : FastTypeTag](fileName: String) = {
//    val file: InputStream = util.Util.getStreamFromClassPathOrFile(fileName)
//    val objAsString: String = Source.fromInputStream(file).getLines().mkString("")
//    val unpickler: JSONPickle = JSONPickle(objAsString)
//    unpickler.unpickle[T]
//  }
//
//  def deserialize[T: Unpickler : FastTypeTag](obj: JSONPickleFormat#PickleType) = obj.unpickle[T]
//}
//
//object BinarySerializer {
//
//  import scala.pickling._
//  import binary._
//  import java.io.{FileInputStream, IOException, FileOutputStream}
//
//  def serialize[T: SPickler : FastTypeTag](t: T) = t.pickle
//
//  def serializeToFile[T: SPickler : FastTypeTag](t: T, fileName: String) = {
//    var out = None: Option[FileOutputStream]
//    try {
//      val buffer = new ByteArrayBuffer()
//      t.pickleTo(buffer)
//      out = Some(new FileOutputStream(fileName))
//      out.get.write(buffer.toArray)
//    } catch {
//      case e: IOException => e.printStackTrace
//    } finally {
//      if (out.isDefined) out.get.close
//    }
//  }
//  //
//  def deserializeFromFile[T: Unpickler : FastTypeTag](fileName: String) = {
//    var value = None: Option[T]
//    var in = None: Option[FileInputStream]
//    try {
//      in = Some(new FileInputStream(fileName))
//
//      val inBuffer = new Array[Byte](in.get.available())
//      in.get.read(inBuffer)
//      val unpicklerArray: BinaryPickle = BinaryPickle(inBuffer)
//      value = Some(unpicklerArray.unpickle[T])
//
//    }
//    catch {
//      case e: IOException => e.printStackTrace
//    }
//    finally {
//      if (in.isDefined) in.get.close
//    }
//    value.get
//  }
//
//  def deserialize[T: Unpickler : FastTypeTag](bytes: Array[Byte]) = bytes.unpickle[T]
//}




