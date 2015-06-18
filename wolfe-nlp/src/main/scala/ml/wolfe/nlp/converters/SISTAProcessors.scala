package ml.wolfe.nlp.converters

import java.util.Properties

import edu.arizona.sista.processors.{Document => SistaDocument, Sentence => SistaSentence}
import edu.arizona.sista.processors.corenlp.{CoreNLPProcessor, CoreNLPDocument}
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import ml.wolfe.nlp.{Document => WolfeDocument, Sentence => WolfeSentence, Token => WolfeToken}
import ml.wolfe.nlp.ie.CorefAnnotation
import scala.collection.JavaConversions._


/**
 * Convenience methods for processing NLP documents.
 *
 * @author Sebastian Riedel
 * @author Jason Naradowsky
 */
object SISTAProcessors {

  // The main SISTA wrapper for most CoreNLP processes
  lazy val sistaCoreNLPProcessor = new CoreNLPProcessor(basicDependencies = false)
  // A separate processor for calls to MaltParser wrapper returning basic dependencies
  lazy val maltSistaCoreNLPProcessor = new FastNLPProcessor(useMalt = true)
  // A separate processor for calls to Stanford Neural Parser wrapper returning basic dependencies
  lazy val nnSistaCoreNLPProcessor = new FastNLPProcessor(useMalt = false, useBasicDependencies = true)
  // Another processor for basic dependencies extracted from the Stanford constituent parser
  lazy val basicSistaCoreNLPProcessor = new CoreNLPProcessor(basicDependencies = true)

  /**
   * Applies tokenization and sentence splitting to the text.
   * @param text text to process.
   * @return a document containing sentences with basic tokens.
   */
  def mkDocument(text: String): WolfeDocument = {
    println("making document...")
    val result = sistaCoreNLPProcessor.mkDocument(text)
    val sentences = result.sentences map SISTAConverter.toWolfeSentence
    WolfeDocument(text, sentences)
  }

  /**
   * Applies tokenization, sentence splitting, and parsing to the text.
   * @param text text to process.
   * @return a document containing sentences with basic tokens and parse structure.
   */
  def mkParsedDocument(text: String): WolfeDocument = {
    val result = sistaCoreNLPProcessor.mkDocument(text)
    sistaCoreNLPProcessor.parse(result)
    val sentences = result.sentences map SISTAConverter.toFullWolfeSentence
    WolfeDocument(text, sentences)
  }

  /**
   * Calls the full SISTA CoreNLP pipeline and returns a wolfe document.
   * @param text the text to process.
   * @return a document with full annotation.
   */
  def annotate(text: String): WolfeDocument = {
    val result = sistaCoreNLPProcessor.annotate(text)
    val sentences = result.sentences map SISTAConverter.toWolfeSentence
    val coref = SISTAConverter.toWolfeCoreference(result.coreferenceChains.get).toArray
    WolfeDocument(text, sentences, coref = CorefAnnotation(coref))
  }

  /**
   * Calls the SISTA CoreNLP components as specified by the arguments
   * @param text the text to process
   * @param posTagger part-of-speech tagger
   * @param lemmatizer lemmatizer
   * @param parser constituent and dependency parses
   * @param ner named entity recognition
   * @param coreference coreference resolution
   * @param srl (NOT SUPPORTED BY CoreNLP) semantic role labeling
   * @return fully annotated document
   */

  def annotateWithParse(text: String,
               posTagger: Boolean=false,
               lemmatizer: Boolean=false,
               parser: Option[ParserModel] = None,
               ner: Boolean=false,
               coreference: Boolean=false,
               srl: Boolean = false,
               prereqs: Boolean = false): WolfeDocument = {
    val result = sistaCoreNLPProcessor.mkDocument(text)
    if (posTagger || (prereqs && (coreference || parser.isDefined || ner))) sistaCoreNLPProcessor.tagPartsOfSpeech(result)
    if (parser.isDefined || (prereqs && coreference)) parse(result, parser.get)  //sistaCoreNLPProcessor.parse(result)
    if (lemmatizer || (prereqs && (coreference || ner))) sistaCoreNLPProcessor.lemmatize(result)
    if (ner || (prereqs && coreference)) sistaCoreNLPProcessor.recognizeNamedEntities(result)
    if (srl) ??? // sistaCoreNLPProcessor.labelSemanticRoles(result)
    if (coreference && !prereqs) {
      require(posTagger && lemmatizer && ner && parser.isDefined, "Coreference resolution requires execution of POS tagger, lemmatizer, NER and parser")
      sistaCoreNLPProcessor.resolveCoreference(result)
    }
    SISTAConverter.sistaToWolfeDocument(result, text = text)
  }

  def annotate(text: String, posTagger: Boolean=false, lemmatizer: Boolean=false, parser: Boolean=false,ner: Boolean=false,coreference: Boolean=false,srl: Boolean = false, prereqs: Boolean = false): WolfeDocument = {
    annotateWithParse(text, posTagger, lemmatizer, if (parser) Some(StanfordCollapsedDependency) else None, ner, coreference, srl, prereqs)
  }

//  def parse(doc: WolfeDocument, model: ParserModel = StanfordCollapsedDependency): WolfeDocument = {
//    SISTAConverter.sistaToWolfeDocument(sistaCoreNLPProcessor.mkDocument(doc.source))
//  }

  def parse(doc: SistaDocument, model: ParserModel = StanfordCollapsedDependency): SistaDocument = {
    sistaCoreNLPProcessor.tagPartsOfSpeech(doc)
    sistaCoreNLPProcessor.lemmatize(doc)
    model match {
      case MaltParser => maltSistaCoreNLPProcessor.parse(doc)
      case StanfordBasicDependency => basicSistaCoreNLPProcessor.parse(doc)
      case StanfordCollapsedDependency => sistaCoreNLPProcessor.parse(doc)
      case StanfordNeuralDependency => nnSistaCoreNLPProcessor.parse(doc)
    }
    doc
  }
}

sealed trait ParserModel
case object MaltParser extends ParserModel
case object StanfordBasicDependency extends ParserModel
case object StanfordCollapsedDependency extends ParserModel
case object StanfordNeuralDependency extends ParserModel








// def main(args: Array[String]): Unit = {
//    val sent = "the quick brown fox jumped over the lazy dog ."
//    val tokens = sent.split(" ").map(w => WolfeToken(word = w))
//    parse(tokens.map(_.word))
//    annotate(sent,  ner = true, parser = true, prereqs = true)
//  }