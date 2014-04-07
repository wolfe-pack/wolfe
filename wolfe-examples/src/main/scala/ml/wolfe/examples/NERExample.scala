package ml.wolfe.examples

import java.io.{InputStream, BufferedInputStream, FileNotFoundException, File}
import scala.io.{BufferedSource, Source}
import ml.wolfe.util.Util
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import ml.wolfe.util.NLP._
import scala.collection.mutable.ListBuffer

/**
 * Created by rockt on 07/04/2014.
 */
object NERExample {
  //http://www.nactem.ac.uk/tsujii/GENIA/ERtask/report.html

  def main(args: Array[String]) {
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

    def streamToLines(stream: InputStream) = {
      val in = new BufferedInputStream(stream)
      val gzIn = new GzipCompressorInputStream(in)
      val tarIn = new TarArchiveInputStream(gzIn)
      var entry = tarIn.getNextEntry

      var lines = Array[String]()

      while (entry != null) {
        if (entry.getName.endsWith("2.iob2")) {
          val content = new Array[Byte](entry.getSize.toInt)
          tarIn.read(content, 0, entry.getSize.toInt)
          val text = new String(content)
          lines = text.split("\n")
        }
        entry = tarIn.getNextEntry
      }

      lines
    }

    def generateDataStructures(docs: Seq[Seq[String]]): Seq[Seq[Sentence]] = {
      for (doc <- docs) yield {
        (for (sentence <- doc.mkString("\n").split("\n\n")) yield {
          Sentence(
            for {
              line <- sentence.split("\n")
              if !line.startsWith("###MEDLINE:")
            } yield {
              val Array(word, label) = line.split("\t")
              Token(word, label, "")
            }
          )
        }).toSeq
      }
    }

    val trainDocs = generateDataStructures(groupLines(streamToLines(trainSource).toIterator, "###MEDLINE:"))
    val testDocs = generateDataStructures(groupLines(streamToLines(testSource).toIterator, "###MEDLINE:"))

    for {
      doc <- trainDocs ++ testDocs
      sentence <- doc
    } println(sentence)
  }
}
