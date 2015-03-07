package ml.wolfe.nlp

import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity._
import edu.berkeley.nlp.entity.coref._
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.entity.preprocess.{SentenceSplitter => BerkeleySentenceSplitter, PreprocessingDriver}
import edu.berkeley.nlp.entity.preprocess.PreprocessingDriver.loadParser
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.syntax.Tree
import edu.berkeley.nlp.util.{Logger}
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

/**
 * @author mbosnjak
 * @author narad
 */
object BerkeleyProcessors {
  // Path to read the sentence splitter model from
  val sentenceSplitterModelPath: String = "wolfe-nlp/models/sentsplit.txt.gz"
  // Path to read the Berkeley Parser grammar from
  val grammarPath: String = "wolfe-nlp/models/eng_sm6.gr"
  // Path to read a backoff grammar from
  val backoffGrammarPath: String = "wolfe-nlp/models/eng_sm1.gr"
  // Path to read the NER model from
  val nerModelPath: String = ""

  Logger.logss("Loading sentence splitter")
  val splitter: BerkeleySentenceSplitter = BerkeleySentenceSplitter.loadSentenceSplitter(sentenceSplitterModelPath)
  Logger.logss("Loading parser")
  val parser: CoarseToFineMaxRuleParser = loadParser(grammarPath)
  Logger.logss("Loading backoff parser")
  val backoffParser: CoarseToFineMaxRuleParser = loadParser(backoffGrammarPath)
  //Logger.logss("Loading NER system")
  val nerSystem: NerSystemLabeled = if (nerModelPath.isEmpty()) null else NerSystemLabeled.loadNerSystem(nerModelPath)



//  // Skip sentence splitting entirely.
//  val skipSentenceSplitting: Boolean = false
//  // Respect line breaks for sentence segmentation (i.e. a new line always means a new sentence). False by default.
//  val respectInputLineBreaks: Boolean = false
//  // Respect two consecutive line breaks for sentence segmentation (i.e. a blank line always means a new sentence). True by default.
//  val respectInputTwoLineBreaks: Boolean = true
//  // Use an alternate tokenizer that may respect the original input a little bit more.
//  val useAlternateTokenizer: Boolean = false




  // Path to read/write the model
  val modelPath: String = "wolfe-nlp/models/coref-onto.ser.gz"

  def toWolfeConstituentTree(tree: Tree[String]): ConstituentTree = {
    if (!tree.isEmpty)
      treeToTree(tree)
    else
      ConstituentTree.empty
  }

  def treeToTree(tree: Tree[String]): ConstituentTree = {
    if (tree.isPreTerminal)
      new ConstituentTree(new PreterminalNode(label = tree.getLabel, word = tree.getChildren.head.getLabel))
    else
      new ConstituentTree(new NonterminalNode(label = tree.getLabel, head = -1), tree.getChildren.map(treeToTree(_)).toList)
  }



////  what it looks like when berkeley parser parses...without memorizing offsets :S
//  def BerkeleyPreprocessing(text: String) = {
//    val lines = Array(text)
//    val canonicalizedParagraphs: Array[String] = splitter.formCanonicalizedParagraphs(lines, respectInputLineBreaks, respectInputTwoLineBreaks)
//
//    val sentences =
//      if (skipSentenceSplitting)
//        canonicalizedParagraphs
//      else
//        splitter.splitSentences(canonicalizedParagraphs)
//
//    val tokenizedSentences: Array[Array[String]] =
//      if (useAlternateTokenizer)
//        BerkeleySentenceSplitter.tokenizeAlternate(sentences)
//      else
//        BerkeleySentenceSplitter.tokenize(sentences)
//
//    tokenizedSentences
//  }



  def mkParsedDocument(text: String, coref: Boolean = true) = {
    val result = SISTAProcessors.mkDocument(text)
    val updatedSentences = result.sentences.map{sentence =>
      val tokenizedSentenceArray = sentence.tokens.map(token => token.word)
      val ctree = PreprocessingDriver.parse(parser, backoffParser, tokenizedSentenceArray.toList)
      sentence.copy(syntax = new SyntaxAnnotation(tree = toWolfeConstituentTree(ctree), dependencies = null))
    }
    Document(text, updatedSentences)
  }


  def annotate(text: String): Document = {
    mkParsedDocument(text)
  }

}


object BerkeleyProcessorTest extends App {
  val doc = BerkeleyProcessors.annotate("Jim said he is going to the store. He lied. He will end up in hell for lying.")
  println(doc)
}
