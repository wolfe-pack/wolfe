import scala.collection.mutable

trait Key[T]

case object Word extends Key[String]
case object Tag extends Key[String]

class CoreNLPMap {


  val map = new mutable.HashMap[Key[_], Any]()

  def get[T](key: Key[T]): T = map(key).asInstanceOf[T]

  def update[T](key: Key[T], value: T) = map(key) = value
}

class CoreNLPToken extends CoreNLPMap {
  def word = get(Word)

}


val coreToken = new CoreNLPMap
coreToken(Word) = "Hello"
coreToken(Tag) = "UH"

trait CoreNLPTrait {
  def get[T](key:Key[T]):T = ???
}
case class MutantToken(word: String, posTag: String, features: CoreNLPMap = null)

case class MutantToken2(word:String) extends CoreNLPTrait

val mt2 = MutantToken2("word")
val mt1 = MutantToken("word","NN")


trait TokenX {
  def word:String
}
trait HasPosTag {
  def posTag:String
}
trait HasLemma {
  def lemma:String
}

case class JasonsTagToken(word:String,posTag:String) extends TokenX with HasPosTag

def tag(tokens:List[TokenX]):List[Token with HasPosTag] = ???






case class Token1(word: String,
                  lemma: String,
                  posTag: String,
                  stem: String,
                  ner: String,
                  number: String,
                  chunkTag: String)

case class MorphFeatures(stem: String, lemma: String, number: String)
case class Token2(word: String, morph: Option[MorphFeatures] = None)

val t2 = Token2("word", None)
t2.morph.get.lemma
for (m <- t2.morph) yield m.lemma

val t3 = Token2("word", Some(MorphFeatures(???, ???, ???)))

class Token(val word: String) {

}
class TagToken(val tag: String, word: String) extends Token(word)

