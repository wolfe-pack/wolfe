package ml.wolfe.apps.UrbanPrediction

import scala.io.Source
import ml.wolfe.nlp._
import java.io.{FileOutputStream, FileWriter, File}

/**
 * Created by marziehsaeidi on 20/11/2014.
 */
object UrbanRegression extends App {

  def getFiles(): Array[File] = {
    val dirName = "/Users/marziehsaeidi/Documents/UrbanIntegrated/Project/Data/London/scrape/yahoo/text_2"
    val dir = new File(dirName)
    val files: Array[File] = dir.listFiles().filter(file => file.getName().endsWith(".txt"))
    files
  }

  val fileName  = "/Users/marziehsaeidi/Documents/UrbanIntegrated/Project/Data/London/scrape/yahoo/text_2/E2.txt"
  val areasFile = "/Users/marziehsaeidi/Documents/UrbanIntegrated/Project/Data/London/areas.txt"
  val outDir    = "/Users/marziehsaeidi/Documents/UrbanIntegrated/Project/Data/London/"
  val adjFile   = "adjectives.txt"
  val advFile   = "adverbs.txt"
  val compFile  = "comparatives.txt"
  val superFile = "superlatives.txt"

  def getAreas(): Array[String] = {
    val areas = Source.fromFile(areasFile).getLines().toArray
    areas
  }

  def read(fileName: String) = {
    val areas = getAreas()

    val files = getFiles()
    files.foreach(file => {
      val content = Source.fromFile(file.getPath).getLines().toArray.filterNot(par => par.trim.isEmpty)

      // get all sentences
      var allSentences = Array[String]()
      content.foreach(par => {
        val sentences = par.replaceAll("\\?+", "\\.").replaceAll("\\!+", "\\.").replaceAll("\\.+", "\\.").split("\\.")
        allSentences ++= sentences
      })

      var adjectives = Array[String]()
      var adverbs = Array[String]()
      var comparatives = Array[String]()
      var superlatives = Array[String]()

      println("sentences:" + allSentences.length)
      //annotate all sentences
      var i = 10
      allSentences.foreach(sentence => {
        //        println(sentence)
          try {
            val doc = SISTAProcessors.annotate(sentence, posTagger = true, lemmatizer = true, parser = true, ner = true, srl = true, coreference = false)
            val tokens = doc.sentences.map(_.tokens).flatten
            val annotatedSent = new StringBuilder()
            tokens.foreach(token => {
              annotatedSent.append(token.word).append("[").append(token.posTag).append("]").append(" ")
            })



            val poss = tokens.map(_.posTag)
            if (poss.contains("VBZ") && poss.contains("JJ")) {
              val vbzInd = poss.indexOf("VBZ")
              val jjInd = poss.indexOf("JJ")
            }


            val adjs = tokens.filter(token => token.posTag.equals("JJ")).map(_.word)
            val advs = tokens.filter(token => token.posTag.equals("RB")).map(_.word)
            val comps = tokens.filter(token => token.posTag.equals("RBR")).map(_.word)
            val supers = tokens.filter(token => token.posTag.equals("RBS")).map(_.word)

            adjectives ++= adjs
            adverbs ++= advs
            comparatives ++= comps
            superlatives ++= supers
            i += 1
            if (i % 10 == 0) println(i)
          } catch {
            case e: Exception => println("error for sentence: " + sentence + "\n" + e.getMessage)
          }
      })

      println("\n\n\n=========>>>>>>writing .......=======\n\n\n")
      writeOrAppendToFile(outDir + adjFile, adjectives.mkString("\n"))
      writeOrAppendToFile(outDir + advFile, adverbs.mkString("\n"))
      writeOrAppendToFile(outDir + compFile, comparatives.mkString("\n"))
      writeOrAppendToFile(outDir + superFile, superlatives.mkString("\n"))

      println("\n\n\n=========>>>>>>wrote file\n\n\n")

    })
  }

  /**
   * Read all areas' text and create files with annotated POS
   */
  def annotateForAllAreas() = {
    val files = getFiles()
    val areasWithSpace = getAreas().filter(area=>area.contains(" "))

    files.foreach(file => {
      writeToFile(outDir + file.getName, "")
      println(file.getName)
      var contentText = Source.fromFile(file.getPath).getLines().toArray.filterNot(par => par.trim.isEmpty).mkString("___")
      areasWithSpace.foreach(area=>{
        println(area + ":" + area.replace(" ",""))
        contentText = contentText.replace(area, area.replace(" ", ""))
      })
      val paragraphs = contentText.split("___")
      // get all sentences
      var allSentences = Array[String]()
      var allAnnotatedSentences = Array[String]()
      paragraphs.foreach(par => {
        val sentences = par.replaceAll("\\?+", "\\.").replaceAll("\\!+", "\\.").replaceAll("\\.+", "\\.").split("\\.")
        allSentences ++= sentences
      })
      println(allSentences.length)
      var i = 0
      allSentences.foreach(sentence => {
        //        println(sentence)
        try {
          val doc = SISTAProcessors.annotate(sentence, posTagger = true, lemmatizer = true, parser = true, ner = true, srl = true, coreference = false)
          val annotated = getSentenceWithPos(doc)
          allAnnotatedSentences ++= annotated
          i+=1

          if (i%100==0) {
            println(i)
            println("==================>>>>>>>> writing to file")
            writeOrAppendToFile(outDir + file.getName, allAnnotatedSentences.mkString("\n"))
            allAnnotatedSentences = Array[String]()
          }

        } catch {
          case e: Exception => println("Exception: " + sentence + " => " + e.getMessage)
        }
      })
      writeOrAppendToFile(outDir + file.getName, allAnnotatedSentences.mkString("\n"))
    })
  }

  /**
   * Convert sentences of a document to text with added POSs.
   * @param doc
   * @return
   */
  def getSentenceWithPos(doc: Document): Array[String] = {
    println(doc.sentences(0).toText)
    val areas = getAreas().map(_.toLowerCase)
    var annotatedSentences = Array[String]()
    doc.sentences.foreach(sentence => {
      val tokensPos = sentence.tokens.map(token => {
        if (areas.contains(token.word.toLowerCase)) {
          token.word + "[LOCATION-" + token.posTag + "]"
        } else {
          token.word + "[" + token.posTag + "]"
        }
      })
      annotatedSentences +:= tokensPos.mkString(" ")
    })
    annotatedSentences
  }

  def writeOrAppendToFile(fileName: String, content: String) {
    if (new File(fileName).exists()) {
      val fw = new FileWriter(fileName, true); //the true will append the new data
      fw.write(content.toString()); //appends the string to the file
      fw.close();
    } else {
      val out = new FileOutputStream(fileName)
      out.write(content.toString().getBytes)
      out.flush()
      out.close()
    }
  }

  def writeToFile(fileName: String, content: String) {
      val out = new FileOutputStream(fileName)
      out.write(content.toString().getBytes)
      out.flush()
      out.close()
  }

  def test() {
    val text = "I think Brixton is nice"
    val doc = SISTAProcessors.annotate(text, posTagger = true, lemmatizer = true, parser = true, ner = true, srl = true, coreference = false)
    val tokens = doc.sentences.map(_.tokens).flatten
    val annotatedSent = new StringBuilder()
    tokens.foreach(token => {
      annotatedSent.append(token.word).append("[").append(token.posTag).append("]").append(" ")
    })
    println(annotatedSent)
  }
  test()
//  annotateForAllAreas()

}
