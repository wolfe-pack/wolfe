package ml.wolfe.examples

import java.io.{InputStream, BufferedInputStream, FileNotFoundException}
import ml.wolfe.util._
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import ml.wolfe.{MaxProduct, Wolfe}
import cc.factorie.optimize.{AveragedPerceptron, OnlineTrainer}
import ml.wolfe.macros.{Library, OptimizedOperators}

/**
 * Created by rockt on 07/04/2014.
 *
 * Linear-chain example for BioNLP 2004 NER corpus.
 * See http://www.nactem.ac.uk/tsujii/GENIA/ERtask/report.html
 *
 * TODO: compare corpus statistics to http://www.nactem.ac.uk/tsujii/GENIA/ERtask/shared_task_intro.pdf
 */
object NERExample {
  import Wolfe._
  import OptimizedOperators._
  import NLP.{Sentence, Chunk, Tag, Token, groupLines}
  //import NLP._
  import Library._

  implicit val defaultChunks = Seq("?").map(c => Chunk(Symbol(c)))
  implicit val labels = Seq("O", "B-protein", "I-protein", "B-cell_type", "I-cell_type", "B-DNA", "I-DNA",
    "B-cell_line", "I-cell_line", "B-RNA", "I-RNA").map(t => Tag(Symbol(t)))

  @Atomic
  def tokenToFeatures(token: Token, prefix: String = ""): Wolfe.Vector = {
    oneHot(prefix + 'word -> token.word.toLowerCase) +
    oneHot(prefix + 'firstCap, I(token.word.head.isUpper)) +
    oneHot(prefix + 'allCap, I(token.word.matches("[A-Z]+"))) +
    oneHot(prefix + 'realNumber, I(token.word.matches("[-0-9]+[.,]+[0-9.,]+"))) +
    oneHot(prefix + 'isDash, I(token.word.matches("[-–—−]"))) +
    oneHot(prefix + 'isQuote, I(token.word.matches("[„“””‘’\"']"))) +
    oneHot(prefix + 'isSlash, I(token.word.matches("[/\\\\]"))) +
    oneHot(prefix + 'prefix2 -> token.word.take(2)) +
    oneHot(prefix + 'suffix2 -> token.word.takeRight(2))
  }

  @Atomic
  def labelToFeature(label: Tag): Wolfe.Vector = {
    oneHot('label -> label)
    //oneHot('iob -> label.label.head)
  }

  def Sentences = Wolfe.all(Sentence)(seqs(all(Token)))

  def observed(s: Sentence) = s.copy(tokens = s.tokens.map(_.copy(tag = hidden)))
  
  def features(s: Sentence): Wolfe.Vector = {
    //token features
    sum { over(0 until s.tokens.size) of (i => tokenToFeatures(s.tokens(i)) outer labelToFeature(s.tokens(i).tag)) } +
    //first order transitions
    sum { over(0 until s.tokens.size - 1) of (i => oneHot('transition -> s.tokens(i).tag -> s.tokens(i + 1).tag)) } +
    //offset conjunctions
    sum { over(1 until s.tokens.size) of (i => tokenToFeatures(s.tokens(i - 1), "@-1") outer labelToFeature(s.tokens(i).tag)) } +
    sum { over(2 until s.tokens.size) of (i => tokenToFeatures(s.tokens(i - 2), "@-2") outer labelToFeature(s.tokens(i).tag)) } +
    sum { over(0 until s.tokens.size - 1) of (i => tokenToFeatures(s.tokens(i + 1), "@+1") outer labelToFeature(s.tokens(i).tag)) } +
    sum { over(0 until s.tokens.size - 2) of (i => tokenToFeatures(s.tokens(i + 2), "@+2") outer labelToFeature(s.tokens(i).tag)) }
  }

  @OptimizeByInference(MaxProduct(_, 1))
  def model(w: Vector)(s: Sentence) = w dot features(s)
  def predictor(w: Vector)(s: Sentence) = argmax { over(Sentences) of model(w) st evidence(observed)(s) }

  @OptimizeByLearning(new OnlineTrainer(_, new AveragedPerceptron, 20, -1))
  def loss(data: Iterable[Sentence])(w: Vector) = sum { over(data) of (s => model(w)(predictor(w)(s)) - model(w)(s)) }
  def learn(data:Iterable[Sentence]) = argmin { over[Vector] of loss(data) }

  def main(args: Array[String]) {
    val useSample = if (args.length > 0) args(0).toBoolean else false

    import scala.sys.process._

    def loadGenia(path: String): InputStream = Util.getStreamFromClassPathOrFile(path)

    val trainPath = "ml/wolfe/datasets/genia/Genia4ERtraining.tar.gz"
    val testPath = "ml/wolfe/datasets/genia/Genia4ERtest.tar.gz"

    //if genia corpus is not present, download it
    val (trainSource, testSource) =
      try {
        (loadGenia(trainPath), loadGenia(testPath))
      } catch {
        case f: FileNotFoundException =>
          "wget http://www.nactem.ac.uk/tsujii/GENIA/ERtask/Genia4ERtraining.tar.gz -P wolfe-examples/src/main/resources/ml/wolfe/datasets/genia/".!!
          "wget http://www.nactem.ac.uk/tsujii/GENIA/ERtask/Genia4ERtest.tar.gz -P wolfe-examples/src/main/resources/ml/wolfe/datasets/genia/".!!
          val prefix = "wolfe-examples/src/main/resources/"
          (loadGenia(prefix + trainPath), loadGenia(prefix + testPath))
      }

    val (train, test) =
      if (useSample) {
        val sample = IOBToWolfe(groupLines(loadIOB(trainSource, "sampletest2").toIterator, "###MEDLINE:")).flatten
        val (trainSample, testSample) = sample.splitAt((sample.size * 0.9).toInt)
        (trainSample, testSample)
      } else
        (IOBToWolfe(groupLines(loadIOB(trainSource).toIterator, "###MEDLINE:")).flatten,
         IOBToWolfe(groupLines(loadIOB(testSource).toIterator, "###MEDLINE:")).flatten)

    println(
      s"""
        |Train sentences: ${train.size}
        |Test sentences:  ${test.size}
      """.stripMargin)

    val w = learn(train)

    def evaluate(corpus: Seq[Sentence]) = {
      val predicted = map { over(corpus) using predictor(w) }
      val evaluated = MentionEvaluator.evaluate(corpus, predicted)
      println(evaluated)
    }

    println("Train:")
    evaluate(train)
    println("Test:")
    evaluate(test)
  }

  def loadIOB(stream: InputStream, sampleFilePrefix: String = "") = {
    val in = new BufferedInputStream(stream)
    val gzIn = new GzipCompressorInputStream(in)
    val tarIn = new TarArchiveInputStream(gzIn)
    var entry = tarIn.getNextEntry

    var lines = Array[String]()

    while (entry != null) {
      val (prefix, suffix) =  if (!sampleFilePrefix.isEmpty) (sampleFilePrefix, ".iob2") else ("Genia", "2.iob2")
      if (entry.getName.startsWith(prefix) && entry.getName.endsWith(suffix)) {
        println("Loading " + entry.getName + " ...")
        val content = new Array[Byte](entry.getSize.toInt)
        tarIn.read(content, 0, entry.getSize.toInt)
        val text = new String(content)
        lines = text.split("\n")
      }
      entry = tarIn.getNextEntry
    }

    lines
  }

  def IOBToWolfe(docs: Seq[Seq[String]]): Seq[Seq[Sentence]] = {
    for (doc <- docs) yield {
      (for (sentence <- doc.mkString("\n").split("\n\n")) yield {
        Sentence(
          for {
            line <- sentence.split("\n")
            if !line.startsWith("###MEDLINE:") && !line.isEmpty
          } yield {
            val Array(word, label) = line.split("\t")
            //FIXME: chunk ("?") shouldn't be needed here, but toString of default value throws Exception
            Token(word, label, '?)
          }
        )
      }).toSeq
    }
  }
}