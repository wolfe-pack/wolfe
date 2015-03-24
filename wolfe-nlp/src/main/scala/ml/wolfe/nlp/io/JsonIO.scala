package ml.wolfe.nlp.io

import ml.wolfe.nlp.{Sentence, CharOffsets, Token}
import org.json4s.JsonAST.JArray
import org.json4s.reflect.TypeInfo
import org.json4s.{Extraction, MappingException, Formats, Serializer}

/**
 * @author Sebastian Riedel
 */
object JsonIO {

  class IndexedSeqSerializer extends Serializer[IndexedSeq[_]] {
    def deserialize(implicit format: Formats) = {
      case (TypeInfo(clazz, ptype), json) if classOf[IndexedSeq[_]].isAssignableFrom(clazz) => json match {
        case JArray(xs) =>
          val t = ptype.getOrElse(throw new MappingException("parameterized type not known"))
          xs.map(x => Extraction.extract(x, TypeInfo(t.getActualTypeArguments()(0).asInstanceOf[Class[_]], None))).toIndexedSeq
        case x => throw new MappingException("Can't convert " + x + " to IndexedSeq")
      }
    }

    def serialize(implicit format: Formats) = {
      case i: IndexedSeq[_] => JArray(i.map(Extraction.decompose).toList)
    }
  }

  def main(args: Array[String]) {
    import org.json4s._
    import org.json4s.jackson.Serialization
    import org.json4s.jackson.Serialization.{read, write}
    implicit val formats = Serialization.formats(NoTypeHints) + new IndexedSeqSerializer
    val token = Token("the", CharOffsets(0, 3))
    val sentence = Sentence(IndexedSeq(token))
    val ser = write(sentence)

    println(ser)

    val deser = read[Sentence](ser)
    println(deser)
    println(write(sentence))
  }
}
