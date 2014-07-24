package ml.wolfe.nlp


import scala.collection.mutable
import scala.pickling._

/**
 * @author Sebastian Riedel
 */
object AttributePickling {
//  def main(args: Array[String]) {
//    import binary._
//
//    implicit val ap = new AttributePickling()
//    ap.register(Lemma)
//
//    val attr = Attributes(Lemma -> "test")
//
//    val p1 = implicitly[SPickler[Map[String, Any]]]
//    val p2 = implicitly[DPickler[Map[String, Any]]]
//    println(p1)
//    //
//    //
//    val pickled = attr.pickle
//    println(pickled.value)
//    val unpickled = pickled.unpickle[Attributes]
//
//    println(unpickled)
//
//    //    val data = Sentence(Seq(Token("the", CharOffsets(0,3), attributes = Attributes(Lemma -> "blah"))))
//    //    val pickledData = data.pickle
//    //    val unpickledData = pickledData.unpickle[Sentence]
//    //    println(unpickledData)
//    //    println(pickledData.value)
//
//    //    val binaryPickle = data.tokens.head.pickle(binary.pickleFormat)
//    //    val binaryUnpickled = binaryPickle.unpickle[Token]
//    //    println(binaryUnpickled)
//
//
//  }
}

class AttributePickling(implicit val format: PickleFormat, val mapPickler: SPickler[Map[String, Any]], val mapUnpickler: Unpickler[Map[String, Any]]) extends SPickler[Attributes] with Unpickler[Attributes] {
  private val picklers        = new mutable.HashMap[Key[_], SPickler[(String, Any)]]
  private val unpicklers      = new mutable.HashMap[Key[_], Unpickler[(String, Any)]]
  private val tags            = new mutable.HashMap[Key[_], FastTypeTag[_]]
  private val keys            = new mutable.HashMap[String, Key[_]]
  private val attributesTag   = implicitly[FastTypeTag[Attributes]]
  private val stringTag       = implicitly[FastTypeTag[String]]
  private val stringPickler   = implicitly[SPickler[String]]
  private val stringUnpickler = implicitly[Unpickler[String]]


  def register[T](key: Key[T])(implicit pickler: SPickler[(String, T)], unpickler: Unpickler[(String, T)], tag: FastTypeTag[(String, T)]) {
    picklers(key) = pickler.asInstanceOf[SPickler[(String, Any)]]
    unpicklers(key) = unpickler.asInstanceOf[Unpickler[(String, Any)]]
    tags(key) = tag
    keys(key.toString) = key
  }
  def pickle(picklee: Attributes, builder: PBuilder) = {

  }
  def unpickle(tag: => FastTypeTag[_], preader: PReader) = {
    Attributes.empty
  }
}

//object PickleTest {
//
//  case class Wrapper(map: Map[String, Any])
//
//  implicit def mkWrapperPickler(implicit mapPickler: SPickler[Map[String, Any]],
//                                pf: PickleFormat) =
//    new SPickler[Wrapper] with Unpickler[Wrapper] {
//      def pickle(picklee: Wrapper, builder: PBuilder) = {
//        builder.beginEntry(picklee)
//        builder.putField("map", b => mapPickler.pickle(picklee.map, b))
//        builder.endEntry()
//      }
//      def unpickle(tag: => FastTypeTag[_], reader: PReader) = {
//        reader.beginEntry()
//        val map = reader.readField("map").unpickle[Map[String, Any]]
//        reader.endEntry()
//        new Wrapper(map)
//      }
//      val format: PickleFormat = pf
//    }
//
//  def main(args: Array[String]) {
//    import binary._
//
//    val wrapper = new Wrapper(Map("1" -> 1, "2" -> "text"))
//    val pickled = wrapper.pickle
//    val unpickled = pickled.unpickle[Wrapper]
//
//    println(unpickled)
//
//    assert(wrapper == unpickled)
//  }
//}