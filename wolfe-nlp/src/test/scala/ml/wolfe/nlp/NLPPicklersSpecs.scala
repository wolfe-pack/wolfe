package ml.wolfe.nlp

import ml.wolfe.WolfeSpec
import ml.wolfe.nlp.ie.EntityMention
import ml.wolfe.nlp.syntax.{Arc, DependencyTree}

/**
 * Specs for pickling Wolfe-NLP data structures
 * Trivial for now, but should be revisited when
 * trickier picklers are implemented (ConstituentTree)
 *
 * Created by narad on 18/08/15.
 */
class NLPPicklersSpecs extends WolfeSpec {

  import scala.pickling.Defaults._
  import ml.wolfe.nlp.io.SyntaxPickler.syntaxPickler
  import ml.wolfe.nlp.io.IEPickler.iePickler

  val tokens = IndexedSeq(
    new Token(word = "the",       posTag = "DET", offsets = CharOffsets(0, 3)),
    new Token(word = "cat",       posTag = "NN",  offsets = CharOffsets(4, 7)),
    new Token(word = "scratched", posTag = "VB",  offsets = CharOffsets(8, 17)),
    new Token(word = "the",       posTag = "DET", offsets = CharOffsets(18, 21)),
    new Token(word = "man",       posTag = "NN",  offsets = CharOffsets(22, 25)),
    new Token(word = "with",      posTag = "PP",  offsets = CharOffsets(26, 30)),
    new Token(word = "claws",     posTag = "NN",  offsets = CharOffsets(31, 36)))

  val dtree = DependencyTree(tokens = tokens, arcs = Seq(
    Arc(child = 0, parent = 1, label = Some("DET")),
    Arc(child = 1, parent = 2, label = Some("SUBJ")),
    Arc(child = 3, parent = 4, label = Some("DET")),
    Arc(child = 4, parent = 2, label = Some("OBJ")),
    Arc(child = 5, parent = 2, label = Some("PREP")),
    Arc(child = 6, parent = 5, label = Some("PREP"))))

  val entities = IndexedSeq(
    EntityMention(label = "PERSON", start = 0, end = 2),
    EntityMention(label = "PERSON", start = 3, end = 5)
  )

  "A syntax pickler" should {
    "store and load dependency trees in json" in {
      import scala.pickling.json._

      val pkl = SyntaxAnnotation(tree = null, dependencies = dtree).pickle
      val result = pkl.unpickle[SyntaxAnnotation]
      result.dependencies should equal(dtree)
    }

    "store and load dependency trees in binary" in {
      import scala.pickling.binary._

      val pkl = SyntaxAnnotation(tree = null, dependencies = dtree).pickle
      val result = pkl.unpickle[SyntaxAnnotation]
      result.dependencies should equal(dtree)
    }
  }

  "An IE pickler" should {
    "store and load a sequence of IE fields in json" in {
      import scala.pickling.json._

      val ie = IEAnnotation(entityMentions = entities, relationMentions = IndexedSeq())
      val pkl = ie.pickle
      val result = pkl.unpickle[IEAnnotation]
      result should equal(ie)
    }

    "store and load dependency trees in binary" in {
      import scala.pickling.binary._

      val ie = IEAnnotation(entityMentions = entities, relationMentions = IndexedSeq())
      val pkl = ie.pickle
      val result = pkl.unpickle[IEAnnotation]
      result should equal(ie)
    }
  }

}
