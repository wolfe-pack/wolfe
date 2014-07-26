import ml.wolfe.nlp._
import scala.collection.mutable
import scala.pickling._
import json._

val attr = Attributes(Lemma -> "blah")
val map = Map(Lemma -> "the")

implicit val ser = new AttributePickling()

ser.register(Lemma)

val wire = attr.pickle.value
val wireMap = Lemma.pickle.value
val unpickled = attr.pickle.unpickle[Attributes]

