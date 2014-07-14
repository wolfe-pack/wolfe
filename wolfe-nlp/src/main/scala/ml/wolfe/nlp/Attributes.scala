package ml.wolfe.nlp

/**
 * A typed key of an attribute
 * @tparam T the type of the attribute value.
 */
trait Key[T] {
  override def toString = getClass.getSimpleName
}

/**
 * Key for lemma attribute.
 */
case object Lemma extends Key[String]

/**
 * Typed map of attributes.
 */
trait Attributes {
  def get[T](key:Key[T]):Option[T]
  def apply[T](key:Key[T]) = get(key).get
  def add[T](key:Key[T],value:T):Attributes
  def keys:Iterable[Key[_]]
  def addOpt[T](key:Key[T],opt:Option[T]):Attributes = opt match {
    case Some(value) => this add (key,value)
    case None => this
  }
  override def toString = {
    keys map (k => s"$k -> ${apply(k)}") mkString ", "
  }
}
/**
 * Companion object to Attributes to provide builders etc.
 */
object Attributes {

  class MapBasedAttributes(map:Map[Key[_],Any]) extends Attributes {
    def get[T](key: Key[T]) = map.get(key).asInstanceOf[Option[T]]
    def add[T](key: Key[T], value: T) = new MapBasedAttributes(map + (key -> value))
    def keys = map.keys
  }

  val empty = new Attributes {
    def get[T](key: Key[T]) = None
    def add[T](key: Key[T], value: T) = new MapBasedAttributes(Map(key -> value))
    def keys = Iterable.empty
  }

  def apply(pairs:(Key[_],Any)*):Attributes = new MapBasedAttributes(pairs.toMap)
}

