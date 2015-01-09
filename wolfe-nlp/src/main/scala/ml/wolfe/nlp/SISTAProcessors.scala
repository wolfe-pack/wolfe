package ml.wolfe.nlp

import edu.arizona.sista.processors.corenlp.{CoreNLPDocument, CoreNLPProcessor}

/**
 * Convenience methods for processing NLP documents.
 *
 * @author Sebastian Riedel
 */
object SISTAProcessors {

  lazy val sistaCoreNLPProcessor = new CoreNLPProcessor()

  /**
   * Applies tokenization and sentence splitting to the text.
   * @param text text to process.
   * @return a document containing sentences with basic tokens.
   */
  def mkDocument(text: String): Document = {
    val result = sistaCoreNLPProcessor.mkDocument(text)
    val sentences = result.sentences map SISTAConverter.toWolfeSentence
    Document(text, sentences)
  }

  /**
   * Applies tokenization, sentence splitting, and parsing to the text.
   * @param text text to process.
   * @return a document containing sentences with basic tokens and parse structure.
   */
  def mkParsedDocument(text: String): Document = {
    val result = sistaCoreNLPProcessor.mkDocument(text)
    sistaCoreNLPProcessor.parse(result)
    val sentences = result.sentences map SISTAConverter.toFullWolfeSentence
    Document(text, sentences)
  }

  /**
   * Calls the full SISTA CoreNLP pipeline and returns a wolfe document.
   * @param text the text to process.
   * @return a document with full annotation.
   */
  def annotate(text: String): Document = {
    val result = sistaCoreNLPProcessor.annotate(text)
    val sentences = result.sentences map SISTAConverter.toWolfeSentence
    val coref = SISTAConverter.toWolfeCoreference(result.coreferenceChains.get)
    Document(text, sentences, coref = CorefAnnotation(coref))
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

  def annotate(text: String,
               posTagger: Boolean=false,
               lemmatizer: Boolean=false,
               parser: Boolean=false,
               ner: Boolean=false,
               coreference: Boolean=false,
               srl: Boolean = false,
               prereqs: Boolean = false): Document = {
    val result = sistaCoreNLPProcessor.mkDocument(text)
    if (posTagger || (prereqs && (coreference || parser))) sistaCoreNLPProcessor.tagPartsOfSpeech(result)
    if (parser || (prereqs && coreference)) sistaCoreNLPProcessor.parse(result)
    if (lemmatizer || (prereqs && coreference)) sistaCoreNLPProcessor.lemmatize(result)
    if (ner || (prereqs && coreference)) sistaCoreNLPProcessor.recognizeNamedEntities(result)
    // NO SRL SUPPORT IN CoreNLP
    // if (srl) sistaCoreNLPProcessor.labelSemanticRoles(result)
    if (coreference && !prereqs) {
      require(posTagger && lemmatizer && ner && parser, "Coreference resolution requires execution of POS tagger, lemmatizer, NER and parser")
      sistaCoreNLPProcessor.resolveCoreference(result)
    }
    val sentences = result.sentences map SISTAConverter.toFullWolfeSentence
    val corefSeq = result.coreferenceChains.map(c => SISTAConverter.toWolfeCoreference(c))
    Document(text, sentences, coref = corefSeq.map(CorefAnnotation(_)).getOrElse(CorefAnnotation.empty))
  }

}
