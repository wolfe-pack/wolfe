package ml.wolfe.neural.io

import java.io.{DataInputStream, File, InputStream}
import cc.factorie.la.{Tensor => TensorTrait}
import ml.wolfe.neural.{FactorieTensor, FactorieMatrix, FactorieVector}

/**
 * User: rockt
 * Date: 11/20/13
 * Time: 5:33 PM
 */

object IDXReader {
  def getInputStreamFromZip(fileName: String): InputStream = {
    val uri = this.getClass.getResource(fileName).toURI
    val file = new File(uri)
    val zipFile = new java.util.zip.ZipFile(file)
    //we assume there is only one file in the archive
    zipFile.getInputStream(zipFile.entries().nextElement())
  }

  def read(fileName: String): TensorTrait = {
    val dis = new DataInputStream(getInputStreamFromZip(fileName))
    dis.skipBytes(2) //we know the first two bytes don't tell us anything

    val readFunction: () => AnyVal = dis.readByte() match {
      case 0x08 => dis.readUnsignedByte
      case 0x09 => dis.readByte
      case 0x0B => dis.readShort
      case 0x0C => dis.readInt
      case 0x0D => dis.readFloat
      case 0x0E => dis.readDouble
      case typ => throw new IllegalStateException(s"I don't know the type $typ")
    }

    val rank = dis.readByte()
    val dims = (0 until rank).map(i => dis.readInt()).toArray

    implicit def anyToDouble(value: AnyVal): Double = value match {
      case b: Byte => b.toDouble
      case s: Short => s.toDouble
      case i: Int => i.toDouble
      case f: Float => f.toDouble
      case d: Double => d
    }

    def readVector(dim: Int): FactorieVector = {
      val vector = new FactorieVector(dim)
      (0 until dim) foreach (ix => {
        val res = readFunction()
        if (res != 0) vector update (ix, res) //sparse
      })
      vector
    }

    def readMatrix(dim1: Int, dim2: Int): FactorieMatrix = {
      val matrix = new FactorieMatrix(dim1, dim2)
      (0 until dim1) foreach (ix => matrix update (ix, readVector(dim2)))
      matrix
    }

    def readTensor(dim1: Int, dim2: Int, dim3: Int): FactorieTensor = {
      val tensor = new FactorieTensor(dim1, dim2, dim3)
      (0 until dim1) foreach (ix => tensor update (ix, readMatrix(dim2, dim3)))
      tensor
    }

    rank match {
      case 1 => readVector(dims(0))
      case 2 => readMatrix(dims(0), dims(1))
      case 3 => readTensor(dims(0), dims(1), dims(2))
      case r => throw new IllegalStateException(s"I can't handle rank $r tensors")
    }
  }
}

//TODO: IDXWriter