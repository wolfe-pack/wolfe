package ml.wolfe.nlp.io

import java.io.DataInputStream
import java.io.IOException
import java.io._
import java.util.HashMap
import java.util.Map
import java.util.StringTokenizer
import java.util.zip.GZIPInputStream

import scala.collection.mutable.ArrayBuffer

/**
 * Created by narad on 3/15/15.
 */
object Word2VecReader {
  private val MAX_SIZE: Int = 50

  def main(args: Array[String]) {
    try {
      loadGoogleBinary(args(0))
    }
    catch {
      case e: Exception => {
        println(e.getStackTrace.mkString("\n"))
      }
    }
    System.out.println("Done.")
  }

  /**
   * Loads the google model
   *
   * @param path the path to the google model
   * @return the loaded model
   * @throws IOException
   */
  @throws(classOf[IOException])
  def loadGoogleBinary(path: String, normalize: Boolean = true): Word2Vec = {
    var dis: DataInputStream = null
    var bis: BufferedInputStream = null
    var gis: GZIPInputStream = null
    var len: Double = 0
    var vector: Float = 0
    var words: Int = 0
    var size: Int = 0
    val w2v: Word2Vec = new Word2Vec

    try {
      if (path.endsWith(".gz")) {
        gis = new GZIPInputStream(new FileInputStream(path))
        dis = new DataInputStream(gis)
      }
      else {
        bis = new BufferedInputStream(new FileInputStream(path))
        dis = new DataInputStream(bis)
      }

      words = readString(dis).toInt
      size = readString(dis).toInt
      println("Words = " + words + "; size = " + size)
      var word: String = null
      val vector = new ArrayBuffer[Float]

      for (i <- 0 until words) {
        word = readString(dis)
        for (j <- 0 until size) {
          vector += readFloat(dis)
        }
        println(word) // + ": " + vector)
      }
      val sum = vector.sum
      w2v.put(word, vector.toArray.map(_ / sum))
     // dis.readChar()

    }
    catch {
      case e: Exception => println(e.getStackTrace.mkString("\n"))
    } finally {
      bis.close
      dis.close
    }
    w2v
  }

  /**
   * Read a string from a data input stream
   * Credit to: https://github.com/NLPchina/Word2VEC_java/blob/master/src/com/ansj/vec/Word2VEC.java
   *
   * @param dis
   * @return
   * @throws IOException
   */
  @throws(classOf[IOException])
  private def readString(dis: DataInputStream): String = {
    var bytes: Array[Byte] = new Array[Byte](MAX_SIZE)
    var b: Byte = dis.readByte
    var i: Int = -1
    val sb: StringBuilder = new StringBuilder
    while (b != 32 && b != 10) {
      i += 1
      bytes(i) = b
      b = dis.readByte
      if (i == 49) {
        sb.append(new String(bytes))
        i = -1
        bytes = new Array[Byte](MAX_SIZE)
      }
    }
    sb.append(new String(bytes, 0, i + 1))
    return sb.toString
  }

  /**
   * Credit to: https://github.com/NLPchina/Word2VEC_java/blob/master/src/com/ansj/vec/Word2VEC.java
   *
   * @param is
   * @return
   * @throws IOException
   */
  @throws(classOf[IOException])
  def readFloat(is: InputStream): Float = {
    val bytes: Array[Byte] = new Array[Byte](4)
    is.read(bytes)
    return getFloat(bytes)
  }

  /**
   * Read float from byte array, credit to:
   * Credit to: https://github.com/NLPchina/Word2VEC_java/blob/master/src/com/ansj/vec/Word2VEC.java
   *
   * @param b
   * @return
   */
  def getFloat(b: Array[Byte]): Float = {
    var accum: Int = 0
    accum = accum | (b(0) & 0xff) << 0
    accum = accum | (b(1) & 0xff) << 8
    accum = accum | (b(2) & 0xff) << 16
    accum = accum | (b(3) & 0xff) << 24
    return java.lang.Float.intBitsToFloat(accum)
  }
}

/*
import java.io.FileInputStream
import java.util.zip.GZIPInputStream



object Word2VecReader {

  def main(args: Array[String]): Unit = {
    for (a <- loadFromFile(args(0))) {
      println(a)
    }
  }

  def loadFromFile(filename: String) = {
    val is = new GZIPInputStream(new FileInputStream(filename))
//    while (is.r)
    Stream.continually(is.read()).takeWhile(-1 !=).map(_.toByte).toArray
  }

}

*/


/*
// Copyright 2013
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
//
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.


import java.io.{File, FileInputStream}
import java.nio.{MappedByteBuffer, ByteBuffer}
import java.nio.ByteOrder._
import java.nio.channels.FileChannel.MapMode._

import scala.collection.mutable.ListBuffer

// Tools to read the binary word models produced by the Google word2vec library (https://code.google.com/p/word2vec/).
//
// Usage:
//   val vocab = Vocab.loadFromFile(filename, 100000)
//   vocab.bestMatches("burger")

case class WordVector(vec: List[Float]) {
  def +(that: WordVector): WordVector = WordVector(this.vec.zip(that.vec).map{case (x, y) => x + y})
  def -(that: WordVector): WordVector = WordVector(this.vec.zip(that.vec).map{case (x, y) => x - y})
  def *(that: WordVector): Double = this.vec.zip(that.vec).map{case (x, y) => x * y}.sum
}

case class Vocab(
                  words: Int,
                  l1_size: Int,
                  vocab: scala.collection.mutable.Map[String, WordVector] = scala.collection.mutable.Map[String, WordVector]()) {

  override def toString(): String = "Vocab with " + vocab.size + " entries"
  def apply(word: String) = vocab.get(word).getOrElse(WordVector(List.fill(l1_size)(0.0f)))

  def bestMatches(word: String, num: Int = 50): List[(String, Double)] = {
    val wordVec = this(word)
    vocab.toList.filterNot(_._1 == word).map(w => (w._1, w._2 * wordVec)).sortBy(_._2).reverse.take(num)
  }
}

object Word2VecReader {

  def main(args: Array[String]): Unit = {
    loadFromFile(args(0))
  }

  // Load a vocabulary from the given file (in binary format), limiting to at most "limit" words (for memory reasons).
  def loadFromFile(filename: String, limit: Int = -1): Vocab = {
    val (fh, buffer) = getFileBuffer(filename)

    val words = readInt(buffer)
    val vectorSize = readInt(buffer)
    println(words + " words in vocab")
    println(vectorSize + " word vector size")

    val vocab = Vocab(words, vectorSize)
    while (buffer.hasRemaining && vocab.vocab.size <= words && (vocab.vocab.size <= limit || limit == -1)) {
      vocab.vocab += (readString(buffer) -> readWordVector(buffer, vectorSize))
      if (vocab.vocab.size % 5000 == 0) println("Loaded " + vocab.vocab.size + " words")
    }

    println("Done, loaded " + vocab.vocab.size + " words")
    fh.close()
    vocab
  }

  // Open a file and return the FileInputStream and ByteBuffer associated with it.
  private def getFileBuffer(filename: String): (FileInputStream, ByteBuffer) = {
    val file = new File(filename)
    val fileSize = file.length.toLong
    val stream = new FileInputStream(file)
    println(fileSize)
    val buffer: MappedByteBuffer = stream.getChannel.map(READ_ONLY, 0.toLong, fileSize)
    buffer.order(LITTLE_ENDIAN)
    (stream, buffer)
  }

  // Read bytes from the buffer until stopFunc returns false.
  private def readBytesUntil(buffer: ByteBuffer, stopFunc: (Byte) => Boolean): List[Byte] = {
    val lst = ListBuffer[Byte]()
    var c: Byte = 0
    while ( {
      c = buffer.get; stopFunc(c)
    }) {
      lst += c
    }
    lst.toList
  }

  // Read a single int value from the buffer.
  private def readInt(buffer: ByteBuffer): Int = {
    readBytesUntil(buffer, c => (c >= 48 && c <= 57)).map(_.toChar).mkString.toInt
  }

  // Read a string composed of normal keyboard characters (non-control characters).
  private def readString(buffer: ByteBuffer): String = {
    readBytesUntil(buffer, c => (c >= 33 && c <= 126)).map(_.toChar).mkString
  }

  // Read a single word vector from the buffer.  Word vectors are encoded as binary floats,
  // followed by a "\n" to separate the records.
  private def readWordVector(buffer: ByteBuffer, length: Int): WordVector = {
    val lst = ListBuffer[Float]()
    1 to length foreach { _ => lst += buffer.getFloat}
    buffer.get // Read off the last "\n" character separating the records in the binary format.

    // Normalize the values in the vector.
    val len = math.sqrt(lst.map(x => x * x).sum).toFloat

    WordVector(lst.map(x => x / len).toList)
  }
}

*/