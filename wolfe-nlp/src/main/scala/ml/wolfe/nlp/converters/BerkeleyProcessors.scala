package ml.wolfe.nlp.converters

import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity._
import edu.berkeley.nlp.entity.coref._
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.entity.preprocess.PreprocessingDriver.loadParser
import edu.berkeley.nlp.entity.preprocess.{PreprocessingDriver, SentenceSplitter => BerkeleySentenceSplitter}
import edu.berkeley.nlp.syntax.Tree
import edu.berkeley.nlp.util.Logger
import ml.wolfe.nlp._
import ml.wolfe.nlp.ie.{CorefAnnotation, CorefMention}
import ml.wolfe.nlp.syntax.{ConstituentTree, NonterminalNode, PreterminalNode}

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

/**
 * @author mbosnjak
 * @author narad
 */
object BerkeleyProcessors {
  // Path to read/write the model
  val modelPath: String = "wolfe-nlp/models/coref-onto.ser.gz"
  // Path to read the sentence splitter model from
  val sentenceSplitterModelPath: String = "wolfe-nlp/models/sentsplit.txt.gz"
  // Path to read the Berkeley Parser grammar from
  val grammarPath: String = "wolfe-nlp/models/eng_sm6.gr"
  // Path to read a backoff grammar from
  val backoffGrammarPath: String = "wolfe-nlp/models/eng_sm1.gr"
  // Path to read the NER model from
  val nerModelPath: String = ""
  // path to Bergsma Lin data
  val numberGenderDataPath: String = "wolfe-nlp/data/gender.data"


  lazy val splitter: BerkeleySentenceSplitter = {
    Logger.logss("Loading sentence splitter")
    BerkeleySentenceSplitter.loadSentenceSplitter(sentenceSplitterModelPath)
  }
  lazy val parser: CoarseToFineMaxRuleParser = {
    Logger.logss("Loading parser")
    loadParser(grammarPath)
  }
  lazy val backoffParser: CoarseToFineMaxRuleParser = {
    Logger.logss("Loading backoff parser")
    loadParser(backoffGrammarPath)
  }
  lazy val nerSystem: NerSystemLabeled = {
    Logger.logss("Loading NER system")
    if (nerModelPath.isEmpty()) null else NerSystemLabeled.loadNerSystem(nerModelPath)
  }
  lazy val numberGenderComputer = {
    NumberGenderComputer.readBergsmaLinData(numberGenderDataPath)
  }

//  // Skip sentence splitting entirely.
//  val skipSentenceSplitting: Boolean = false
//  // Respect line breaks for sentence segmentation (i.e. a new line always means a new sentence). False by default.
//  val respectInputLineBreaks: Boolean = false
//  // Respect two consecutive line breaks for sentence segmentation (i.e. a blank line always means a new sentence). True by default.
//  val respectInputTwoLineBreaks: Boolean = true
//  // Use an alternate tokenizer that may respect the original input a little bit more.
//  val useAlternateTokenizer: Boolean = false

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

  def mkParsedDocument(text: String) = {
    val result = SISTAProcessors.mkDocument(text)
    val updatedSentences = result.sentences.map{sentence =>
      val tokenizedSentenceArray = sentence.tokens.map(token => token.word)
      val ctree = PreprocessingDriver.parse(parser, backoffParser, tokenizedSentenceArray.toList)
      sentence.copy(syntax = new SyntaxAnnotation(tree = toWolfeConstituentTree(ctree), dependencies = null))
    }
    Document(text, updatedSentences)
  }


  def annotate(text: String, coref: Boolean = true): Document = {
    if (!coref) {
      mkParsedDocument(text)
    } else {
      val result = SISTAProcessors.mkDocument(text)
      val tokenizedSentences = result.sentences.map(_.tokens.map(_.word).toArray).toArray
      val conll = PreprocessingDriver.renderDocConllLines("**", tokenizedSentences, parser, backoffParser, nerSystem)
      val reader = new ConllDocReader(Language.ENGLISH, "")
      val doc = reader.assembleConllDoc(ArrayBuffer(conll.map{x => ArrayBuffer(x :_*)} :_*), "**", 0);

      val assembler = CorefDocAssembler(Driver.lang, Driver.useGoldMentions)
      val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer))

      val corefDoc = if (Driver.useCoordination)
        assembler.createCorefDocWithCoordination(doc, mentionPropertyComputer)
      else
        assembler.createCorefDoc(doc, mentionPropertyComputer)

      val docGraph = new DocumentGraph(corefDoc, false)
      val pairwiseScorer = GUtil.load(modelPath).asInstanceOf[PairwiseScorer]
      val allPredBackptrsAndPredClusterings = CorefSystem.runPredict(Seq(docGraph), pairwiseScorer, false)

      val clustering = allPredBackptrsAndPredClusterings(0)._2.bind(docGraph.getMentions, false)

      val ret = for ((mention, clusterID) <- clustering.ments.zipWithIndex) yield {
        CorefMention(clustering.clustering.getClusterIdx(clusterID), mention.sentIdx, mention.startIdx, mention.endIdx, mention.headIdx)
      }
      Document(text, result.sentences, coref = CorefAnnotation(ret))
    }
  }

}


object BerkeleyProcessorTest extends App {
  val doc = BerkeleyProcessors.annotate("Jim said he is going to the store. He lied. He will end up in hell for lying. Why Jim, why did you lie?", true)
  println(doc)
//  val doc2 = BerkeleyProcessors.test("Jim said he is going to the store. He lied. He will end up in hell for lying. Why Jim, why did you lie?", true)
//  println(doc2)
}
