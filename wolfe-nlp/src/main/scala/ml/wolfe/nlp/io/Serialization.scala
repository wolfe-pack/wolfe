package event.io

import ml.wolfe.nlp.syntax.DependencyTree
import ml.wolfe.nlp._
import scala.pickling.binary.StreamOutput
import scala.pickling.pickler._
import scala.pickling._
import scala.xml.Elem
import java.io.{FileInputStream, FileOutputStream, File}

import scala.pickling.Defaults._
import scala.pickling.binary._

/**
 * Created by narad on 09/08/15.
 */
object NLPPicklers {

  import DocumentPickler._

  def lazyPickle(iter: Iterator[Document], file: File): Unit = {
    val outputStream = new FileOutputStream(file)
    val output = new StreamOutput(outputStream)
    iter.foreach(_.pickleTo(output))
    outputStream.close
  }

  def lazyUnpickle(file: File) = new Iterator[Document] {
    val inputStream = new FileInputStream(file)
    val streamPickle = BinaryPickle(inputStream)

    override def hasNext: Boolean = inputStream.available > 0

    override def next(): Document = streamPickle.unpickle[Document]
  }
}

object DocumentPickler {

  import SentencePickler._

  implicit val documentPickler: Pickler[Document] with Unpickler[Document] =
    new Pickler[Document] with Unpickler[Document] with PrimitivePicklers with PrimitiveArrayPicklers {

      def pickle(picklee: Document, builder: PBuilder) = {
        builder.beginEntry(picklee)
        builder.putField("source", b => {
          b.hintTag(implicitly[FastTypeTag[String]])
          b.hintStaticallyElidedType()
          stringPickler.pickle(picklee.source, b)
        })

        builder.putField("id", b => {
          b.hintTag(implicitly[FastTypeTag[String]])
          b.hintStaticallyElidedType()
          stringPickler.pickle(picklee.id.getOrElse("None"),b)
        })
        builder.putField("sentences", b => {
          b.hintTag(implicitly[FastTypeTag[IndexedSeq[Sentence]]])
          b.hintStaticallyElidedType()
          indexedSeqPickler[Sentence].pickle(picklee.sentences, b)
        })
        builder.endEntry()
      }

      def tag = FastTypeTag[Document]

      def unpickle(tag: String, reader: PReader) = {
        val readerSource = reader.readField("source")
        readerSource.hintTag(implicitly[FastTypeTag[String]])
        readerSource.hintStaticallyElidedType()

        val tagSource = readerSource.beginEntry()
        val source = stringPickler.unpickle(tagSource, readerSource).asInstanceOf[String]
        readerSource.endEntry()

        val readerID = reader.readField("id")
        readerID.hintTag(implicitly[FastTypeTag[String]])
        readerID.hintStaticallyElidedType()

        val tagID = readerID.beginEntry()
        val id = stringPickler.unpickle(tagID,readerID).asInstanceOf[String]
        readerID.endEntry()

        val readerSentence = reader.readField("sentences")
        readerSentence.hintTag(implicitly[FastTypeTag[IndexedSeq[Sentence]]])
        readerSentence.hintStaticallyElidedType()

        val tagSentence = readerSentence.beginEntry()
        val sentences = indexedSeqPickler[Sentence].unpickle(tagSentence,readerSentence).asInstanceOf[IndexedSeq[Sentence]]
        readerSentence.endEntry()

        new Document(
          source = source,
          sentences = sentences,
          id = if (id == "None") None else Some(id))
      }
    }
}



object SentencePickler {

  import SyntaxPickler._
  import IEPickler._

  implicit val sentencePickler: Pickler[Sentence] with Unpickler[Sentence] =
    new Pickler[Sentence] with Unpickler[Sentence] with PrimitivePicklers {

      def pickle(picklee: Sentence, builder: PBuilder) = {
        builder.beginEntry(picklee)
        builder.putField("tokens", b => {
          b.hintTag(implicitly[FastTypeTag[IndexedSeq[Token]]])
          b.hintStaticallyElidedType()
          indexedSeqPickler[Token].pickle(picklee.tokens, b)
        })
        builder.putField("syntax", b => {
          b.hintTag(implicitly[FastTypeTag[SyntaxAnnotation]])
          b.hintStaticallyElidedType()
          syntaxPickler.pickle(picklee.syntax, b)
        })
        builder.putField("ie", b => {
          b.hintTag(implicitly[FastTypeTag[IEAnnotation]])
          b.hintStaticallyElidedType()
          iePickler.pickle(picklee.ie, b)
        })
        builder.endEntry()
      }

      def tag = FastTypeTag[Sentence]

      def unpickle(tag: String, reader: PReader) = {
        val readerTokens = reader.readField("tokens")
        readerTokens.hintTag(implicitly[FastTypeTag[IndexedSeq[Token]]])
        readerTokens.hintStaticallyElidedType()

        val tagTokens = readerTokens.beginEntry()
        val tokens = indexedSeqPickler[Token].unpickle(tagTokens, readerTokens).asInstanceOf[IndexedSeq[Token]]
        readerTokens.endEntry()

        val readerSyntax = reader.readField("syntax")
        readerSyntax.hintTag(FastTypeTag[SyntaxAnnotation])
        readerSyntax.hintStaticallyElidedType()

        val tagSyntax = readerSyntax.beginEntry()
        val syntax = syntaxPickler.unpickle(tagSyntax, readerSyntax).asInstanceOf[SyntaxAnnotation]
        readerSyntax.endEntry()

        val readerIE = reader.readField("ie")
        readerIE.hintTag(FastTypeTag[IEAnnotation])
        readerIE.hintStaticallyElidedType()

        val tagIE = readerIE.beginEntry()
        val ie = iePickler.unpickle(tagIE, readerIE).asInstanceOf[IEAnnotation]
        readerIE.endEntry()

        Sentence(tokens = tokens, syntax = syntax, ie = ie)
      }
    }
}

object SyntaxPickler {

  implicit val syntaxPickler: Pickler[SyntaxAnnotation] with Unpickler[SyntaxAnnotation] =
    new Pickler[SyntaxAnnotation] with Unpickler[SyntaxAnnotation] with PrimitivePicklers {

      val spickler: Pickler[DependencyTree] = Pickler.generate[DependencyTree]
      val sunpickler: Unpickler[DependencyTree] = Unpickler.generate[DependencyTree]


      def pickle(picklee: SyntaxAnnotation, builder: PBuilder) = {
        spickler.pickle(picklee.dependencies, builder)
      }

      def tag = FastTypeTag[SyntaxAnnotation]

      def unpickle(tag: String, reader: PReader) = {
        val dtree = sunpickler.unpickle(tag, reader).asInstanceOf[DependencyTree]
        SyntaxAnnotation(tree = null, dependencies = dtree)
      }
    }
}

object IEPickler {

  implicit val iePickler: Pickler[IEAnnotation] with Unpickler[IEAnnotation] =
    new Pickler[IEAnnotation] with Unpickler[IEAnnotation] with PrimitivePicklers {

      val spickler: Pickler[IEAnnotation] = Pickler.generate[IEAnnotation]
      val sunpickler: Unpickler[IEAnnotation] = Unpickler.generate[IEAnnotation]


      def pickle(picklee: IEAnnotation, builder: PBuilder) = {
        spickler.pickle(picklee, builder)
      }

      def tag = FastTypeTag[IEAnnotation]

      def unpickle(tag: String, reader: PReader) = {
        sunpickler.unpickle(tag, reader).asInstanceOf[IEAnnotation]
      }
    }
}





//    val dpickle = data.pickle
//    writeToFile(dpickle, file)
//
//    val undpickle: BinaryPickle = loadBinary(file)
//    val undocs = undpickle.unpickle[Array[Document]]
//
//    for (undoc <- undocs) {
//      println("Unpickled Source: " + undoc.source)
//      println("Unpickled ID: " + undoc.id)
//      println("UnPickled Sentences: " + undoc.sentences.mkString("\n"))
//      println(undoc.sentences.size)
//    }