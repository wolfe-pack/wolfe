package ml.wolfe.nlp.io

// Copyright 2013 trananh
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import java.io._
import scala.Array
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/** A simple binary file reader.
  * @constructor Create a binary file reader.
  * @param file The binary file to be read.
  *
  * @author trananh
  */
class VecBinaryReader(val file: File) {

  /** Overloaded constructor */
  def this(filename: String) = this(new File(filename))

  /** ASCII values for common delimiter characters */
  private val SPACE = 32
  private val LF = 10

  /** Open input streams */
  private val fis = new FileInputStream(file)
  private val bis = new BufferedInputStream(fis)
  private val dis = new DataInputStream(bis)

  /** Close the stream. */
  def close() { dis.close(); bis.close(); fis.close() }

  /** Read the next byte.
    * @return The next byte from the file.
    */
  def read(): Byte = dis.readByte()

  /** Read the next token as a string, using the provided delimiters as breaking points.
    * @param delimiters ASCII code of delimiter characters (default to SPACE and LINE-FEED).
    * @return String representation of the next token.
    */
  def readToken(delimiters: Set[Int] = Set(SPACE, LF)): String = {
    val bytes = new ArrayBuffer[Byte]()
    val sb = new StringBuilder()
    var byte = dis.readByte()
    while (!delimiters.contains(byte)) {
      bytes.append(byte)
      byte = dis.readByte()
    }
    sb.append(new String(bytes.toArray[Byte])).toString()
  }

  /** Read next 4 bytes as a floating-point number.
    * @return The floating-point value of the next 4 bytes.
    */
  def readFloat(): Float = {
    // We need to reverse the byte order here due to endian-compatibility.
    java.lang.Float.intBitsToFloat(java.lang.Integer.reverseBytes(dis.readInt()))
  }

}


/** A Scala port of the word2vec model.  This interface allows the user to access the vector representations
  * output by the word2vec tool, as well as perform some common operations on those vectors.  It does NOT
  * implement the actual continuous bag-of-words and skip-gram architectures for computing the vectors.
  *
  * More information on word2vec can be found here: https://code.google.com/p/word2vec/
  *
  * Example usage:
  * {{{
  * val model = new Word2Vec()
  * model.load("vectors.bin")
  * val results = model.distance(List("france"), N = 10)
  *
  * model.pprint(results)
  * }}}
  *
  * @constructor Create a word2vec model.
  *
  * @author trananh
  */
class Word2Vec {

  /** Map of words and their associated vector representations */
  private val vocab = new mutable.HashMap[String, Array[Float]]()

  /** Number of words */
  private var numWords = 0

  /** Number of floating-point values associated with each word (i.e., length of the vectors) */
  private var vecSize = 0

  /** Load data from a binary file.
    * @param filename Path to file containing word projections in the BINARY FORMAT.
    * @param limit Maximum number of words to load from file (a.k.a. max vocab size).
    * @param normalize Normalize the loaded vectors if true (default to true).
    */
  def load(filename: String, limit: Integer = Int.MaxValue, normalize: Boolean = true): Unit = {
    // Check edge case
    val file = new File(filename)
    if (!file.exists()) {
      throw new FileNotFoundException("Binary vector file not found <" + file.toString + ">")
    }

    // Create new reader to read data
    val reader = new VecBinaryReader(file)

    // Read header info
    numWords = Integer.parseInt(reader.readToken())
    vecSize = Integer.parseInt(reader.readToken())
    println("\nFile contains " + numWords + " words with vector size " + vecSize)

    // Read the vocab words and their associated vector representations
    var word = ""
    val vector = new Array[Float](vecSize)
    var normFactor = 1f
    for (_ <- 0 until math.min(numWords, limit)) {
      // Read the word
      word = reader.readToken()

      // Read the vector representation (each vector contains vecSize number of floats)
      for (i <- 0 until vector.length) vector(i) = reader.readFloat()

      // Store the normalized vector representation, keyed by the word
      normFactor = if (normalize) magnitude(vector).toFloat else 1f
      vocab.put(word, vector.map(_ / normFactor) )

      // Eat up the next delimiter character
      reader.read()
    }
    println("Loaded " + math.min(numWords, limit) + " words.\n")

    // Finally, close the reader
    reader.close()
  }

  /** Return the number of words in the vocab.
    * @return Number of words in the vocab.
    */
  def wordsCount: Int = numWords

  /** Size of the vectors.
    * @return Size of the vectors.
    */
  def vectorSize: Int = vecSize

  /** Clear internal data. */
  def clear() {
    vocab.clear()
    numWords = 0
    vecSize = 0
  }

  /** Check if the word is present in the vocab map.
    * @param word Word to be checked.
    * @return True if the word is in the vocab map.
    */
  def contains(word: String): Boolean = {
    vocab.get(word).isDefined
  }

  /** Get the vector representation for the word.
    * @param word Word to retrieve vector for.
    * @return The vector representation of the word.
    */
  def vector(word: String): Array[Float] = {
    vocab.getOrElse(word, Array[Float]())
  }

  /** Compute the Euclidean distance between two vectors.
    * @param vec1 The first vector.
    * @param vec2 The other vector.
    * @return The Euclidean distance between the two vectors.
    */
  def euclidean(vec1: Array[Float], vec2: Array[Float]): Double = {
    assert(vec1.length == vec2.length, "Uneven vectors!")
    var sum = 0.0
    for (i <- 0 until vec1.length) sum += math.pow(vec1(i) - vec2(i), 2)
    math.sqrt(sum)
  }

  /** Compute the Euclidean distance between the vector representations of the words.
    * @param word1 The first word.
    * @param word2 The other word.
    * @return The Euclidean distance between the vector representations of the words.
    */
  def euclidean(word1: String, word2: String): Double = {
    assert(contains(word1) && contains(word2), "Out of dictionary word! " + word1 + " or " + word2)
    euclidean(vocab.get(word1).get, vocab.get(word2).get)
  }

  /** Compute the cosine similarity score between two vectors.
    * @param vec1 The first vector.
    * @param vec2 The other vector.
    * @return The cosine similarity score of the two vectors.
    */
  def cosine(vec1: Array[Float], vec2: Array[Float]): Double = {
    assert(vec1.length == vec2.length, "Uneven vectors!")
    var dot, sum1, sum2 = 0.0
    for (i <- 0 until vec1.length) {
      dot += (vec1(i) * vec2(i))
      sum1 += (vec1(i) * vec1(i))
      sum2 += (vec2(i) * vec2(i))
    }
    dot / (math.sqrt(sum1) * math.sqrt(sum2))
  }

  /** Compute the cosine similarity score between the vector representations of the words.
    * @param word1 The first word.
    * @param word2 The other word.
    * @return The cosine similarity score between the vector representations of the words.
    */
  def cosine(word1: String, word2: String): Double = {
    assert(contains(word1) && contains(word2), "Out of dictionary word! " + word1 + " or " + word2)
    cosine(vocab.get(word1).get, vocab.get(word2).get)
  }

  /** Compute the magnitude of the vector.
    * @param vec The vector.
    * @return The magnitude of the vector.
    */
  def magnitude(vec: Array[Float]): Double = {
    math.sqrt(vec.foldLeft(0.0){(sum, x) => sum + (x * x)})
  }

  /** Normalize the vector.
    * @param vec The vector.
    * @return A normalized vector.
    */
  def normalize(vec: Array[Float]): Array[Float] = {
    val mag = magnitude(vec).toFloat
    vec.map(_ / mag)
  }

  /** Find the vector representation for the given list of word(s) by aggregating (summing) the
    * vector for each word.
    * @param input The input word(s).
    * @return The sum vector (aggregated from the input vectors).
    */
  def sumVector(input: List[String]): Array[Float] = {
    // Find the vector representation for the input. If multiple words, then aggregate (sum) their vectors.
    input.foreach(w => assert(contains(w), "Out of dictionary word! " + w))
    val vector = new Array[Float](vecSize)
    input.foreach(w => for (j <- 0 until vector.length) vector(j) += vocab.get(w).get(j))
    vector
  }

  /** Find N closest terms in the vocab to the given vector, using only words from the in-set (if defined)
    * and excluding all words from the out-set (if non-empty).  Although you can, it doesn't make much
    * sense to define both in and out sets.
    * @param vector The vector.
    * @param inSet Set of words to consider. Specify None to use all words in the vocab (default behavior).
    * @param outSet Set of words to exclude (default to empty).
    * @param N The maximum number of terms to return (default to 40).
    * @return The N closest terms in the vocab to the given vector and their associated cosine similarity scores.
    */
  def nearestNeighbors(vector: Array[Float], inSet: Option[Set[String]] = None,
                       outSet: Set[String] = Set[String](), N: Integer = 40)
  : List[(String, Float)] = {
    // For performance efficiency, we maintain the top/closest terms using a priority queue.
    // Note: We invert the distance here because a priority queue will dequeue the highest priority element,
    //       but we would like it to dequeue the lowest scoring element instead.
    val top = new mutable.PriorityQueue[(String, Float)]()(Ordering.by(-_._2))

    // Iterate over each token in the vocab and compute its cosine score to the input.
    var dist = 0f
    val iterator = if (inSet.isDefined) vocab.filterKeys(k => inSet.get.contains(k)).iterator else vocab.iterator
    iterator.foreach(entry => {
      // Skip tokens in the out set
      if (!outSet.contains(entry._1)) {
        dist = cosine(vector, entry._2).toFloat
        if (top.size < N || top.head._2 < dist) {
          top.enqueue((entry._1, dist))
          if (top.length > N) {
            // If the queue contains over N elements, then dequeue the highest priority element
            // (which will be the element with the lowest cosine score).
            top.dequeue()
          }
        }
      }
    })

    // Return the top N results as a sorted list.
    assert(top.length <= N)
    top.toList.sortWith(_._2 > _._2)
  }

  /** Find the N closest terms in the vocab to the input word(s).
    * @param input The input word(s).
    * @param N The maximum number of terms to return (default to 40).
    * @return The N closest terms in the vocab to the input word(s) and their associated cosine similarity scores.
    */
  def distance(input: List[String], N: Integer = 40): List[(String, Float)] = {
    // Check for edge cases
    if (input.size == 0) return List[(String, Float)]()
    input.foreach(w => {
      if (!contains(w)) {
        println("Out of dictionary word! " + w)
        return List[(String, Float)]()
      }
    })

    // Find the vector representation for the input. If multiple words, then aggregate (sum) their vectors.
    val vector = sumVector(input)

    nearestNeighbors(normalize(vector), outSet = input.toSet, N = N)
  }

  /** Find the N closest terms in the vocab to the analogy:
    * - [word1] is to [word2] as [word3] is to ???
    *
    * The algorithm operates as follow:
    * - Find a vector approximation of the missing word = vec([word2]) - vec([word1]) + vec([word3]).
    * - Return words closest to the approximated vector.
    *
    * @param word1 First word in the analogy [word1] is to [word2] as [word3] is to ???.
    * @param word2 Second word in the analogy [word1] is to [word2] as [word3] is to ???
    * @param word3 Third word in the analogy [word1] is to [word2] as [word3] is to ???.
    * @param N The maximum number of terms to return (default to 40).
    *
    * @return The N closest terms in the vocab to the analogy and their associated cosine similarity scores.
    */
  def analogy(word1: String, word2: String, word3: String, N: Integer = 40): List[(String, Float)] = {
    // Check for edge cases
    if (!contains(word1) || !contains(word2) || !contains(word3)) {
      println("Out of dictionary word! " + Array(word1, word2, word3).mkString(" or "))
      return List[(String, Float)]()
    }

    // Find the vector approximation for the missing analogy.
    val vector = new Array[Float](vecSize)
    for (j <- 0 until vector.length)
      vector(j) = vocab.get(word2).get(j) - vocab.get(word1).get(j) + vocab.get(word3).get(j)

    nearestNeighbors(normalize(vector), outSet = Set(word1, word2, word3), N = N)
  }

  /** Rank a set of words by their respective distance to some central term.
    * @param word The central word.
    * @param set Set of words to rank.
    * @return Ordered list of words and their associated scores.
    */
  def rank(word: String, set: Set[String]): List[(String, Float)] = {
    // Check for edge cases
    if (set.size == 0) return List[(String, Float)]()
    (set + word).foreach(w => {
      if (!contains(w)) {
        println("Out of dictionary word! " + w)
        return List[(String, Float)]()
      }
    })

    nearestNeighbors(vocab.get(word).get, inSet = Option(set), N = set.size)
  }

  /** Pretty print the list of words and their associated scores.
    * @param words List of (word, score) pairs to be printed.
    */
  def pprint(words: List[(String, Float)]) = {
    println("\n%50s".format("Word") + (" " * 7) + "Cosine distance\n" + ("-" * 72))
    println(words.map(s => "%50s".format(s._1) + (" " * 7) + "%15f".format(s._2)).mkString("\n"))
  }

}


/** ********************************************************************************
  * Demo of the Scala ported word2vec model.
  * ********************************************************************************
  */
object RunWord2Vec {

  /** Demo. */
  def main(args: Array[String]) {
    // Load word2vec model from binary file.
    val model = new Word2Vec()
    model.load("../word2vec-scala/vectors.bin")

    // distance: Find N closest words
    model.pprint(model.distance(List("france"), N = 10))
    model.pprint(model.distance(List("france", "usa")))
    model.pprint(model.distance(List("france", "usa", "usa")))

    // analogy: "king" is to "queen", as "man" is to ?
    model.pprint(model.analogy("king", "queen", "man", N = 10))

    // rank: Rank a set of words by their respective distance to the central term
    model.pprint(model.rank("apple", Set("orange", "soda", "lettuce")))
  }

}