package ml.wolfe

import scala.language.implicitConversions

/**
 * @author Sebastian Riedel
 */
package object nlp {

  implicit def toDoc(source:String) = Document(source, Seq(Sentence(Seq(Token(source,CharOffsets(0,source.length))))))

}
