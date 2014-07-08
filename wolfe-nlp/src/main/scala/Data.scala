import scala.collection.immutable.HashMap

trait KeyType[T]

case object Lemma extends KeyType[String]

class AttributeMap extends HashMap[KeyType[_], Any] {
  def blah[T](key: KeyType[T]): Option[T] = this.get(key).asInstanceOf[Option[T]]
}

case class Token(word: String, posTag: String, attributes: AttributeMap)

object DataApp extends App {
  val attr = new AttributeMap()
  //attr += Lemma -> "blahlemma"
  val t = Token("test", "N", attr)
  println(t)
}