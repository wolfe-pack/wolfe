package ml.wolfe.nlp.Data.structures

import ml.wolfe.WolfeSpec
import ml.wolfe.nlp.Data.generics.{GenericDocumentCompanion, GenericSentenceCompanion}

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.{IndexedSeqLike, mutable}

/**
 * Created by ibecker on 23/03/2015.
 */

private case class TokenWithPOS(word: String, offsets: CharOffsets, posTag: POSTag) extends TokenLike with TokenWithPOSLike


private trait SentenceWithTreesLike[T <: TokenLike] extends SentenceLike[T] with IndexedSeq[T] with IndexedSeqLike[T,SentenceWithTreesLike[T]]{
  val trees: String
  override protected def newBuilder: mutable.Builder[T, SentenceWithTreesLike[T]] = SentenceWithTreesLike.newBuilder
}

private object SentenceWithTreesLike extends GenericSentenceCompanion[SentenceWithTreesLike, TokenLike] {
  implicit def canBuildFrom[OldT <: TokenLike, NewT <: TokenLike]: CanBuildFrom[SentenceWithTreesLike[OldT], NewT, SentenceWithTreesLike[NewT]] = newCanBuildFrom
  def empty(): SentenceWithTreesLike[TokenLike] = new SentenceWithTreesLike[TokenLike] {val trees : String                = ""
                                                                                        val tokens: IndexedSeq[TokenLike] = IndexedSeq.empty
  }
  def fromSentence[NewT <: TokenLike](old: SentenceWithTreesLike[_ <: TokenLike], tokens: IndexedSeq[NewT]): SentenceWithTreesLike[NewT] = new SentenceWithTrees[NewT](tokens, old.trees)
}

private case class SentenceWithTrees[T <: TokenLike](tokens: IndexedSeq[T], trees: String) extends SentenceWithTreesLike[T]

private object SentenceWithStringLike extends GenericSentenceCompanion[SentenceWithStringLike, TokenLike] {
  implicit def canBuildFrom[OldT <: TokenLike, NewT <: TokenLike]: CanBuildFrom[SentenceWithStringLike[OldT], NewT, SentenceWithStringLike[NewT]] = newCanBuildFrom
  def empty(): SentenceWithStringLike[TokenLike] = new SentenceWithString[TokenLike](IndexedSeq.empty)
  def fromSentence[NewT <: TokenLike](old: SentenceWithStringLike[_ <: TokenLike], tokens: IndexedSeq[NewT]): SentenceWithStringLike[NewT] = new SentenceWithString[NewT](tokens)
}

private trait SentenceWithStringLike[T <: TokenLike] extends SentenceLike[T] with IndexedSeq[T] with IndexedSeqLike[T,SentenceWithStringLike[T]]{
  override protected def newBuilder: mutable.Builder[T, SentenceWithStringLike[T]] = SentenceWithStringLike.newBuilder
}

private case class SentenceWithString[T <: TokenLike](tokens: IndexedSeq[T]) extends SentenceWithStringLike[T]

private trait DocumentWithTreesLike[S <: SentenceLike[_ <: TokenLike]] extends DocumentLike[S] with IndexedSeq[S] with IndexedSeqLike[S, DocumentWithTreesLike[S]] {
  val trees : String
  override protected def newBuilder: mutable.Builder[S, DocumentWithTreesLike[S]] = DocumentWithTreesLike.newBuilder
}

private object DocumentWithTreesLike extends GenericDocumentCompanion[DocumentWithTreesLike, SentenceLike, TokenLike] {

  implicit def canBuildFrom[OldS <: SentenceLike[_ <: TokenLike], NewS <: SentenceLike[_ <: TokenLike]]: CanBuildFrom[DocumentWithTreesLike[OldS], NewS, DocumentWithTreesLike[NewS]] = newCanBuildFrom
  def empty(): DocumentWithTreesLike[SentenceLike[TokenLike]] = new DocumentWithTrees("", "", IndexedSeq.empty)
  def fromDocument[NewS <: SentenceLike[_ <: TokenLike]](old: DocumentWithTreesLike[_ <: SentenceLike[_ <: TokenLike]], sentences: IndexedSeq[NewS]): DocumentWithTreesLike[NewS] = new DocumentWithTrees[NewS](old.source, old.trees, sentences)
}

private case class DocumentWithTrees[S <: SentenceLike[_ <: TokenLike]](source: String, trees: String, sentences: IndexedSeq[S]) extends DocumentWithTreesLike[S]



class DocumentSpecs extends WolfeSpec {


  "Token Data Structures" when {
    "in their basic structure" should {
      val t1 = Token("Test", CharOffsets(0,4))
      "have the correct type" in {
        t1 shouldBe a[Token]
      }
      "print correctly" in {
        t1.toString shouldBe "Token(Test,CharOffsets(0,4))"
      }
      "print prettily correctly" in {
        t1.toPrettyString shouldBe "Test"
      }
    }
    "if extended with POS tags" should {
      val t1 = TokenWithPOS("Test", CharOffsets(0,4), POSTag("NP"))
      "have the correct type" in {
        t1 shouldBe a[TokenWithPOS]
      }
      "print correctly" in {
        t1.toString shouldBe "TokenWithPOS(Test,CharOffsets(0,4),POSTag(NP))"
      }
      "print prettily correctly" in {
        t1.toPrettyString shouldBe "Test/NP"
      }
    }
  }


  "Sentence Data Structures" when {
    val s1 = Sentence(for (i <- 1 to 5) yield Token(i.toString, CharOffsets(i-1 + (i-1)*2,i + 2*(i-1))))
    "in their initial structures" should {

      "have the correct type" in {
        s1 shouldBe a [Sentence[_]]
      }

      "have the correct token type parameter" in {
        s1.head shouldBe a [Token]
      }

      "print correctly" in {
        s1.toString() shouldBe "Sentence(Token(1,CharOffsets(0,1)), Token(2,CharOffsets(3,4)), Token(3,CharOffsets(6,7)), Token(4,CharO"+
          "ffsets(9,10)), Token(5,CharOffsets(12,13)))"
      }

    }

    "the underlying token type is changed" should {
      val s2 = s1.map({ x => TokenWithPOS(x.word, x.offsets, POSTag("Number")) })

      "have the correct type" in {
        s2 shouldBe a [Sentence[_]]
      }
      "have the correct token type parameter" in {
        s2.head shouldBe a [TokenWithPOS]
      }

      "print correctly" in {
        s2.toString() shouldBe "Sentence(TokenWithPOS(1,CharOffsets(0,1),POSTag(Number)), TokenWithPOS(2,CharOffsets("+
          "3,4),POSTag(Number)), TokenWithPOS(3,CharOffsets(6,7),POSTag(Number)), TokenWithPOS(4,CharOffsets(9,10),POSTag(Numbe"+
          "r)), TokenWithPOS(5,CharOffsets(12,13),POSTag(Number)))"
      }
    }
    

    "the Sentence type is changed" should {
      val s2 = SentenceWithString(s1)

      "have the correct type" in {
        s2 shouldBe a [SentenceWithString[_]]
      }

      "have the correct token type parameter" in {
        s2.head shouldBe a [Token]
      }

      "print correctly" in {
        s2.toString() shouldBe "SentenceWithString(Token(1,CharOffsets(0,1)), Token(2,CharOffsets(3,4)), Token(3,CharOffsets(6,7)),"+
          " Token(4,CharOffsets(9,10)), Token(5,CharOffsets(12,13)))"
      }
    }

    "both Sentence and Token type is changed" should {
      val s2 = SentenceWithString(s1.map({x => TokenWithPOS(x.word, x.offsets, POSTag("Number"))}))
      "have the correct type" in {
        s2 shouldBe a [SentenceWithString[_]]
      }

      "have the correct token type parameter" in {
        s2.head shouldBe a [TokenWithPOS]
      }

      "print correctly" in {
        s2.toString() shouldBe "SentenceWithString(TokenWithPOS(1,CharOffsets(0,1),POSTag(Number)), TokenWithPOS(2,CharOffsets("+
          "3,4),POSTag(Number)), TokenWithPOS(3,CharOffsets(6,7),POSTag(Number)), TokenWithPOS(4,CharOffsets(9,10),POSTag(Numbe"+
          "r)), TokenWithPOS(5,CharOffsets(12,13),POSTag(Number)))"
      }
    }
  }


  "Document Data Structures" when {
    val s1 = for (i <- 1 to 3) yield Sentence(for (j <- i to i +2 ) yield Token(j.toString, CharOffsets(j-1 + (j-1)*2, j + 2*(j-1))))
    val d1 = Document("",s1)

    "in their initial structures" should {

      "have the correct document type" in {
        d1 shouldBe a[Document[_]]
      }
      "have the correct Sentence type" in {
        d1.head shouldBe a[Sentence[_]]
      }
      "have the correct Token type" in {
        d1.head.head shouldBe a[Token]
      }
      "print correctly" in {
        d1.toString() shouldBe "Document(Sentence(Token(1,CharOffsets(0,1)), Token(2,CharOffsets(3,4)), Token(3,CharOffsets(6,7))),"+
          " Sentence(Token(2,CharOffsets(3,4)), Token(3,CharOffsets(6,7)), Token(4,CharOffsets(9,10))), Sentence(Token(3,CharOffsets(6,"+
          "7)), Token(4,CharOffsets(9,10)), Token(5,CharOffsets(12,13))))"
      }

    }
    "Sentence and Token are changed" should {
      val d2 = d1 map {s => SentenceWithString(s map {t => TokenWithPOS(t.word, t.offsets, POSTag("Number"))})}
      "have the correct document type" in {
        d2 shouldBe a[Document[_]]
      }
      "have the correct Sentence type" in {
        d2.head shouldBe a[SentenceWithString[_]]
      }
      "have the correct Token type" in {
        d2.head.head shouldBe a[TokenWithPOS]
      }
      "print correctly" in {
        d2.toString() shouldBe "Document(SentenceWithString(TokenWithPOS(1,CharOffsets(0,1),POSTag(Number)), TokenWithPOS(2"+
          ",CharOffsets(3,4),POSTag(Number)), TokenWithPOS(3,CharOffsets(6,7),POSTag(Number))), SentenceWithString(TokenWithPOS"+
          "(2,CharOffsets(3,4),POSTag(Number)), TokenWithPOS(3,CharOffsets(6,7),POSTag(Number)), TokenWithPOS(4,CharOffsets(9,10),P"+
          "OSTag(Number))), SentenceWithString(TokenWithPOS(3,CharOffsets(6,7),POSTag(Number)), TokenWithPOS(4,CharOffsets(9,10"+
          "),POSTag(Number)), TokenWithPOS(5,CharOffsets(12,13),POSTag(Number))))"
      }
    }

  }
  "Extended Document Structures" when {
    val s1 = for (i <- 1 to 3) yield Sentence(for (j <- i to i +2 ) yield Token(j.toString, CharOffsets(j-1 + (j-1)*2, j + 2*(j-1))))
    val d1 = DocumentWithTrees("", "Trees!", s1)

    "in their initial structures" should {

      "have the correct document type" in {
        d1 shouldBe a[DocumentWithTrees[_]]
      }
      "have the correct Sentence type" in {
        d1.head shouldBe a[Sentence[_]]
      }
      "have the correct Token type" in {
        d1.head.head shouldBe a[Token]
      }
      "print correctly" in {
        d1.toString() shouldBe "DocumentWithTrees(Sentence(Token(1,CharOffsets(0,1)), Token(2,CharOffsets(3,4)), Token(3,CharOffs"+
        "ets(6,7))),"+
        " Sentence(Token(2,CharOffsets(3,4)), Token(3,CharOffsets(6,7)), Token(4,CharOffsets(9,10))), Sentence(Token(3,CharOffsets(6,"+
        "7)), Token(4,CharOffsets(9,10)), Token(5,CharOffsets(12,13))))"
      }

    }
    "Sentence and Token are changed" should {
      val d2 = d1 map {s => SentenceWithString(s map {t => TokenWithPOS(t.word, t.offsets, POSTag("Number"))})}
      "have the correct document type" in {
        d2 shouldBe a[DocumentWithTrees[_]]
      }
      "have the correct Sentence type" in {
        d2.head shouldBe a[SentenceWithString[_]]
      }
      "have the correct Token type" in {
        d2.head.head shouldBe a[TokenWithPOS]
      }
      "print correctly" in {
        d2.toString() shouldBe "DocumentWithTrees(SentenceWithString(TokenWithPOS(1,CharOffsets(0,1),POSTag(Number)), Tok"+
        "enWithPOS(2"+
        ",CharOffsets(3,4),POSTag(Number)), TokenWithPOS(3,CharOffsets(6,7),POSTag(Number))), SentenceWithString(TokenWithPOS"+
        "(2,CharOffsets(3,4),POSTag(Number)), TokenWithPOS(3,CharOffsets(6,7),POSTag(Number)), TokenWithPOS(4,CharOffsets(9,10),P"+
        "OSTag(Number))), SentenceWithString(TokenWithPOS(3,CharOffsets(6,7),POSTag(Number)), TokenWithPOS(4,CharOffsets(9,10"+
        "),POSTag(Number)), TokenWithPOS(5,CharOffsets(12,13),POSTag(Number))))"
      }
    }
  }

}
