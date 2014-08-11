package ml.wolfe.nlp

import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

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
    Document(text, sentences)
  }


}
