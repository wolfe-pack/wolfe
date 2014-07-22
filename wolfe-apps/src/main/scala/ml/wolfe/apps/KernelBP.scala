package ml.wolfe.apps

import java.io.File
import java.util.StringTokenizer

import breeze.linalg._
import ml.wolfe.FactorGraph.{Edge, Node}
import ml.wolfe.nlp.{Key, Document, SISTAProcessors}
import ml.wolfe.{SimpleIndex, Index, BeliefPropagation, FactorGraph}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import ml.wolfe.fg._


/**
 * @author Sebastian Riedel
 */
object KernelBP {

  def main(args: Array[String]) {
    val data_AB = for (i <- 0 until 4) yield (i, 3 - i)
    val data_BC = for (i <- 0 until 4) yield (i, 3 - i)
    val data_AB_A = data_AB.map(_._1)
    val data_AB_B = data_AB.map(_._2)
    val data_BC_B = data_BC.map(_._1)
    val data_BC_C = data_BC.map(_._2)

    val data_A = data_AB_A
    val data_B = data_BC_B

    val kernel_A: (Any, Any) => Double = { case (x1, x2) => if (x1 == x2) 1.0 else 0.0 }
    val kernel_B: (Any, Any) => Double = { case (x1, x2) => if (x1 == x2) 1.0 else 0.0 }
    val kernel_C: (Any, Any) => Double = { case (x1, x2) => if (x1 == x2) 1.0 else 0.0 }

    val model_AB = new EdgeModel(data_AB, kernel_A, kernel_B, 0.1)
    val model_BC = new EdgeModel(data_BC, kernel_B, kernel_C, 0.1)

    //define translations between messages from edges
    val trans_AB_BC = calculateTranslationMatrix(data_AB_B, data_BC_B, kernel_B)
    val trans_BC_AB = trans_AB_BC.t
    val trans_AB_B = calculateTranslationMatrix(data_AB_B, data_B, kernel_B)

    val f_trans_AB_B = (d: DenseVector[Double]) => trans_AB_B * d
    val f_trans_AB_A = (d: DenseVector[Double]) => d
    val f_trans_BC_B = (d: DenseVector[Double]) => d
    val f_trans_AB_BC = (d: DenseVector[Double]) => trans_AB_BC * d
    val f_trans_BC_AB = (d: DenseVector[Double]) => trans_BC_AB * d

    //create the factor graph
    val fg = new FactorGraph
    val n_A = fg.addNode(0)
    val n_B = fg.addNode(0)
    //val n_C = fg.addNode(0) //we don't use this node because it's observed

    val f_AB = fg.addFactor()
    val f_BC = fg.addFactor()

    val e_BC_B = fg.addEdge(f_BC, n_B)
    val e_AB_B = fg.addEdge(f_AB, n_B)
    val e_AB_A = fg.addEdge(f_AB, n_A)

    e_BC_B.msgs = new KernelBPMSgs
    e_AB_B.msgs = new KernelBPMSgs
    e_AB_A.msgs = new KernelBPMSgs

    val obs_C = 2

    f_AB.potential = new KernelBPPairPotential(e_AB_A, e_AB_B, model_AB)
    f_BC.potential = new KernelBPLocalPotential(model_BC, obs_C)

    n_A.variable = new KernelBPVar(Map.empty, Map(e_AB_A -> f_trans_AB_A), data_A.size)
    n_B.variable = new KernelBPVar(
      Map(
        (e_AB_B, e_BC_B) -> f_trans_AB_BC,
        (e_BC_B, e_AB_B) -> f_trans_BC_AB),
      Map(
        e_AB_B -> f_trans_AB_B,
        e_BC_B -> f_trans_BC_B
      ), data_B.size)

    fg.build()

    BeliefPropagation.sumProduct(1, gradientAndObjective = false, schedule = false)(fg)
    println(n_A.variable.asInstanceOf[KernelBPVar].belief)
    println(n_B.variable.asInstanceOf[KernelBPVar].belief)

  }

  def normalizeMsg(msg: DenseVector[Double]) {
    val sum = breeze.linalg.sum(msg)
    if (math.abs(sum) > 0.0)
      msg :*= 1.0 / sum
    else
      msg := 1.0 / msg.length
  }

  def calculateTranslationMatrix(sourceData: Seq[Any], targetData: Seq[Any], targetKernel: (Any, Any) => Double) = {
    val result = new DenseMatrix[Double](targetData.size, sourceData.size)
    for (col <- 0 until sourceData.size; row <- 0 until targetData.size) {
      result(row, col) = targetKernel(sourceData(col), targetData(row))
    }
    result
  }


  class KernelBPVar(val edge2edgeTranslations: Map[(Edge, Edge), DenseVector[Double] => DenseVector[Double]],
                    val edge2nodeTranslation: Map[Edge, DenseVector[Double] => DenseVector[Double]],
                    val dim: Int) extends Var {
    var belief: DenseVector[Double] = null


    override def updateN2F(edge: Edge) = {
      val msgs = edge.msgs.asInstanceOf[KernelBPMSgs]
      msgs.n2f = new DenseVector[Double](Array.fill(dim)(1.0))
      for (other <- edge.n.edges; if other != edge) {
        val source = other.msgs.asInstanceOf[KernelBPMSgs]
        val translation = edge2edgeTranslations(other, edge)
        msgs.n2f :*= translation(source.f2n)
      }
      normalizeMsg(msgs.n2f)
    }
    override def updateMarginalBelief(node: Node) = {
      belief = new DenseVector[Double](Array.fill(dim)(1.0))
      for (edge <- node.edges) {
        val source = edge.msgs.asInstanceOf[KernelBPMSgs]
        val translation = edge2nodeTranslation(edge)
        belief :*= translation(source.f2n)
      }
      normalizeMsg(belief)

    }
  }

  class KernelBPMSgs extends Msgs {
    var f2n: DenseVector[Double] = null
    var n2f: DenseVector[Double] = null

    var f2nOld: DenseVector[Double] = null
    def saveCurrentF2NAsOld() = f2nOld = f2n
  }

  def gram(kernel: (Any, Any) => Double, data: Seq[(Any, Any)], part: ((Any, Any)) => Any) = {
    val G1 = new DenseMatrix[Double](data.size, data.size)
    for (i <- 0 until data.size) {
      val x1 = part(data(i))
      for (j <- i until data.size) {
        val y1 = part(data(j))
        val sim1 = kernel(x1, y1)
        G1(i, j) = sim1
        G1(j, i) = sim1
      }
    }
    G1
  }

  class EdgeModel(val data: Seq[(Any, Any)],
                  val k1: (Any, Any) => Double, val k2: (Any, Any) => Double,
                  val lambda: Double) {

    val G1    = gram(k1, data, _._1)
    val G2    = gram(k2, data, _._2)
    val R     = (DenseMatrix.eye[Double](data.size) * lambda) * data.size.toDouble
    val G1_R  = G1 + R
    val G2_R  = G2 + R
    val T1    = inv(G1_R)
    val T2    = inv(G2_R)
    val Obs12 = inv(G1_R * G2_R)
    val Obs21 = inv(G2_R * G1_R)

  }

  trait KernelBPPotential extends Potential {
    def model: EdgeModel
  }

  class KernelBPPairPotential(val arg1: Edge, val arg2: Edge, val model: EdgeModel) extends KernelBPPotential {


    override def marginalF2N(edge: Edge) = {
      val (other, trans) = if (edge == arg1) (arg2, model.T1) else (arg1, model.T2)
      val pre = other.msgs.asInstanceOf[KernelBPMSgs]
      val msgs = edge.msgs.asInstanceOf[KernelBPMSgs]
      msgs.f2n = trans * pre.n2f
    }
  }


  class KernelBPLocalPotential(val model: EdgeModel, val observation: Any) extends KernelBPPotential {

    val orig = new DenseVector[Double](Array.ofDim[Double](model.data.size))
    for (((x1, x2), i) <- model.data.zipWithIndex) orig(i) = model.k2(x2, observation)
    val msg = model.Obs21 * orig

    override def marginalF2N(edge: Edge) = {
      val msgs = edge.msgs.asInstanceOf[KernelBPMSgs]
      msgs.f2n = msg
    }


  }

}

object KernelBPRelationExtraction {
  def main(args: Array[String]) {
    //load RE file
    //create two kernels: surface pattern, and freebase relation
    //create two edge models: (1) surface pattern -> freebase relation, (2) surface pattern to surface pattern
    //alternatively: use ACE relation extraction data
    //learn relational paraphrasing model on NYT data
    //problem: too much lexical choice, cannot be captured in small dataset
    //solution: use efficient ridge regression on explicit features? For example using gradient methods
    //each component of the target feature space corresponds to one
    //or http://web.stanford.edu/group/SOL/software/lsqr/

  }
}

object KernelBPTed {

  def tokenize(string: String) = {
    val tokenizer = new StringTokenizer(string, " \n")
    val result = new mutable.ListBuffer[String]
    while (tokenizer.hasMoreTokens) result += tokenizer.nextToken()
    result
  }


  def textKernel(a1: Any, a2: Any) = (a1, a2) match {
    case (d1: Document, d2: Document) =>
    //
  }

  object DocVector extends Key[Vector[Double]]
  object WordIndex extends Key[Index]

  def calculateTFIDFVectors(docs: Seq[Document]): Seq[Document] = {
    val index = new SimpleIndex
    val rawDF = new mutable.HashMap[String, Double]() withDefaultValue(0.0)
    val numDocs = docs.size
    //get document frequencies first
    for (doc <- docs) {
      val words = for (t <- doc.tokens) yield t.word

      for (w <- words.distinct) {
        rawDF(w) += 1.0
        index.index(w)
      }
    }
    val numWords = rawDF.size
    val result = for (doc <- docs) yield {
      //get term frequencies
      val rawTF = new mutable.HashMap[String,Double]() withDefaultValue(0.0)
      for (t <- doc.tokens)  {
        rawTF(t.word) += 1.0
      }
      val docVector = SparseVector.zeros[Double](numWords)
      for ((w,f) <- rawTF) {
        val tf = rawTF(w)
        val idf = math.log(numDocs / rawDF(w))
        val wordIndex = index(w)
        docVector(wordIndex) = tf * idf
      }
      doc.copy(attributes = doc.attributes add (DocVector,docVector) add (WordIndex,index))
    }
    result
  }

  def main(args: Array[String]) {
    //load positive and negative training documents for en-de
    val de_en = new File("/Users/sriedel/corpora//ted-cldc/de-en/")
    val en_de = new File("/Users/sriedel/corpora//ted-cldc/en-de/")

    println("Loading translations de")
    val de_files = getTrainSet(de_en, "_de").sortBy(_._1).take(1000) //.map(d => tokenize(d._2))
    println("Loading translations en")
    val en_files = getTrainSet(en_de, "_en").sortBy(_._1).take(1000) //.map(d => tokenize(d._2))
    println(de_files.size)
    println(en_files.size)

    val de_docs = de_files map (f => SISTAProcessors.mkDocument(f._2))
    val en_docs = en_files map (f => SISTAProcessors.mkDocument(f._2))

    println(de_docs.head)

    val de_vecs = calculateTFIDFVectors(de_docs)
    val en_vecs = calculateTFIDFVectors(en_docs)

    println(de_vecs.head.attributes(DocVector))



    //first create basic vectors for each document.
    //get document frequencies


    //get tfidf scores

    //create kernel

    //now create edge model


    //for (dir <- new File())

    //create basic feature representations for each document (use bag of words for now)

    //compress datasets using cholesky
    //LinearAlgebra.cholesky()


  }

  def getTrainSet(topDir: File, toRemove:String = "") = {
    val de_files = for (tag <- new File(topDir, "train").listFiles();
                        posNeg <- tag.listFiles();
                        doc <- posNeg.listFiles()) yield {
      val txt = Source.fromFile(doc).getLines().mkString("\n")
      def removed = if (toRemove == "") txt else txt.replaceAllLiterally(toRemove,"")
      //      val tokens = txt.split(" ").map(_.dropRight(3))
      //      println(tokens.mkString(" "))
      (posNeg.getName, doc.getName) -> removed
    }
    de_files
  }

}

object IncompleteCholesky {

  case class Result(K_chol: DenseMatrix[Double], I: Seq[Int], R: DenseMatrix[Double], W: DenseMatrix[Double], acc: Double)

  def incompleteCholesky[T](X: Seq[T], kernel: (T, T) => Double, eta: Double, power: Double = 1.0, blockSize: Int = 1) = {
    //taken from https://github.com/karlnapf/graphlab/blob/master/apps/kernelbp/python_precompute_kernel_matrices/src/incomplete_cholesky.py

    val m = X.size

    //current list of evaluated kernel values
    var K = DenseMatrix.zeros[Double](m, blockSize)

    //current list of basis indices
    var I = new ListBuffer[Int]

    //??
    val nu = new ListBuffer[Double]

    //current low rank basis
    var R = DenseMatrix.zeros[Double](m, blockSize)

    //current index of basis
    var j = 0

    //diagonal (assumed to be one)
    //todo: shouldn't this be gram matrix diagonal? Only one if kernel (e.g. dot product) is normalized
    val d = DenseVector.ones[Double](m)

    //current error
    var a = max(d)
    I += argmax(d)

    while (a > eta) {
      assert(j < m, "something's fishy")
      nu += math.sqrt(a)
      val current = X(I(j))

      //calculate next column of the kernel matrix
      if (power >= 1) {
        for (i <- 0 until m) K(i, j) = math.pow(kernel(current, X(i)), power)
      } else {
        for (i <- 0 until m) K(i, j) = 0.0
      }

      //product of current R and (transposed) row vector of R corresponding to chosen pivot.
      //each row vector of R corresponds to a linear combination of pivots 0 .. j
      val R_dot_j: DenseVector[Double] =
        if (j == 0) DenseVector.zeros[Double](m)
        else
        if (j == 1) R(::, 0) * R(I(j), 0)
        else
          R(0 until m, 0 until j) * R(I(j), 0 until j).t

      //The new current column vector
      R(::, j) := (K(::, j) - R_dot_j) / nu(j)

      //see how much we have covered each data points
      d := d - breeze.numerics.pow(R(::, j), 2)

      //how far is the least captured data point away?
      a = max(d)

      //what is the least captured data point?
      I += argmax(d)

      //next column
      j += 1

      //expand
      if (j >= K.cols) {
        K = DenseMatrix.horzcat(K, DenseMatrix.zeros[Double](m, blockSize))
        R = DenseMatrix.horzcat(R, DenseMatrix.zeros[Double](m, blockSize))
      }


    }

    //Remove unused columns (possible when blockSize > 1)
    K = K(0 until m, 0 until j)
    R = R(0 until m, 0 until j)

    //don't need the last pivot index
    I = I.dropRight(1)

    //"from low rank to full rank"
    //W=solve(R[:,I], R)
    val Rsquare = R(I, ::).toDenseMatrix
    val Rinv = inv(Rsquare) //is there a better way to solve for many b's?
    val W = R * Rinv //multiplying W with Rsquare gives the complete R

    //low rank K
    val Kchol = K(I, ::).toDenseMatrix


    Result(Kchol, I, R, W, a)


  }

  def main(args: Array[String]) {
    val rand = new util.Random(40l)
    val n = 4
    val basis = for (i <- 0 until n) yield DenseVector.tabulate(0 until n)(x => if (x == i) 1.0 else 0.0)
    val others = for (i <- 0 until n) yield DenseVector(Array.fill(n)(rand.nextDouble()))
    val data = rand.shuffle(basis ++ others).map(v => v / v.norm)

    def kernel(x1: DenseVector[Double], x2: DenseVector[Double]) = x1 dot x2

    val result = incompleteCholesky(data, kernel, 0.001, 1.0, 1)

    println(result.acc)
    println(result.I)
    println()
    println(result.K_chol)
    println()
    println(result.W)
    println()
    println(result.R)


  }

}