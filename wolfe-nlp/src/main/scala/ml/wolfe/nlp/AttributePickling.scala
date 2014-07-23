package ml.wolfe.nlp


import scala.collection.mutable
import scala.pickling._

/**
 * @author Sebastian Riedel
 */
object AttributePickling {
  def main(args: Array[String]) {
    import json._

    implicit val ap = new AttributePickling()
    ap.register(Lemma)

    val attr = Attributes(Lemma -> "test")

    val pickled = attr.pickle
    val unpickled = pickled.unpickle[Attributes]

    val data = Sentence(Seq(Token("the", CharOffsets(0,3), attributes = Attributes(Lemma -> "blah"))))
    val pickledData = data.pickle
    val unpickledData = pickledData.unpickle[Sentence]
    println(unpickledData)
    println(pickledData.value)

    val binaryPickle = data.pickle(binary.pickleFormat)
//    val binaryUnpickled = binaryPickle.unpickle[Sentence]
//    println(binaryUnpickled)


  }
}

class AttributePickling(implicit val format: PickleFormat) extends SPickler[Attributes] with Unpickler[Attributes] {
  private val picklers        = new mutable.HashMap[Key[_], SPickler[(String,Any)]]
  private val unpicklers      = new mutable.HashMap[Key[_], Unpickler[(String,Any)]]
  private val tags            = new mutable.HashMap[Key[_], FastTypeTag[_]]
  private val keys            = new mutable.HashMap[String, Key[_]]
  private val attributesTag   = implicitly[FastTypeTag[Attributes]]
  private val stringTag       = implicitly[FastTypeTag[String]]
  private val stringPickler   = implicitly[SPickler[String]]
  private val stringUnpickler = implicitly[Unpickler[String]]


  def register[T](key: Key[T])(implicit pickler: SPickler[(String,T)], unpickler: Unpickler[(String,T)], tag: FastTypeTag[(String,T)]) {
    picklers(key) = pickler.asInstanceOf[SPickler[(String,Any)]]
    unpicklers(key) = unpickler.asInstanceOf[Unpickler[(String,Any)]]
    tags(key) = tag
    keys(key.toString) = key
  }
  def pickle(picklee: Attributes, builder: PBuilder) = {
    //    builder.beginEntry(picklee)
    val keys = picklee.keys
    builder.hintTag(attributesTag)
    builder.beginEntry(picklee)
    builder.beginCollection(keys.size)
    for (key <- keys; tag <- tags.get(key); pickler <- picklers.get(key)) {
      builder.putElement(b => {
        b.hintTag(tag)
        b.hintStaticallyElidedType()
        pickler.pickle(key.toString -> picklee(key), b)
      })
    }
    builder.endCollection()
    builder.endEntry()

  }
  def unpickle(tag: => FastTypeTag[_], preader: PReader) = {
    preader.hintTag(attributesTag)
    val attrTag = preader.beginEntry()
    val collReader = preader.beginCollection()
    val length = collReader.readLength()
    val resultMap = new mutable.HashMap[Key[_],Any]()
    for (_ <- 0 until length) {
      val elemReader = collReader.readElement()
      val keyReader = elemReader.readField("_1")
      keyReader.hintTag(stringTag)
      keyReader.hintStaticallyElidedType()
      keyReader.beginEntry()
      val keyString = stringUnpickler.unpickle(stringTag,keyReader).asInstanceOf[String]
      for (key <- keys.get(keyString); tag <- tags.get(key); unpickler <- unpicklers.get(key)) {
        val pair = unpickler.unpickle(tag,elemReader).asInstanceOf[(String,Any)]
        resultMap(key) = pair._2
      }
      elemReader.endEntry()
    }
    collReader.endCollection()
    preader.endCollection()
    //r.readField()
    if (resultMap.isEmpty) Attributes.empty else new Attributes.MapBasedAttributes(resultMap.toMap)
  }
}