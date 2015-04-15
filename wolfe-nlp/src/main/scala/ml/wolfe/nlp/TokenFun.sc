import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable, IndexedSeqLike}
import scala.collection.immutable.IndexedSeq

import scala.language.{higherKinds,reflectiveCalls}

case class CharOffsets(start: Int, end: Int)

trait TokenLike[+T <: TokenLike[T]] {
  this: T =>
  val word: String
  def toPrettyString = word
  val offsets: CharOffsets
}
case class Token(word: String, offsets: CharOffsets) extends TokenLike[Token]

case class TokenWithString(word: String, offsets: CharOffsets) extends TokenLike[TokenWithString] {
  def whyNot = word.reverse
}

object wrapper {

  trait SentenceBasis {
    type T <: TokenLike[T]
  }

  trait SentenceLike[S <: SentenceLike[S]] extends SentenceBasis with IndexedSeq[SentenceBasis#T] with IndexedSeqLike[SentenceBasis#T, SentenceLike[S]] {
    this: S =>
    val tokens: IndexedSeq[T]
    def length: Int = tokens.length
    def apply(idx: Int): T = tokens(idx)
    def toText = tokens map (_.word) mkString " "
    def offsets = CharOffsets(tokens.head.offsets.start, tokens.last.offsets.end)
    def toPrettyString = tokens.map(_.toPrettyString).mkString(" ")
    def sentenceCompanion = SentenceLike
    override protected def newBuilder: mutable.Builder[SentenceBasis#T, S] = ???
  }
  case class Sentence[Token <: TokenLike[Token]](tokens: IndexedSeq[Token]) extends SentenceLike[Sentence[Token]] {
    type T = Token
  }

  sealed trait GSC2[ThisSentence[X <: ThisSentence[X]] <: SentenceLike[X]] {

    def newBuilder[NewT <: S#T, S <: ThisSentence[S]]: mutable.Builder[NewT, ThisSentence[NewT]] = IndexedSeq.newBuilder[NewT] mapResult (fromSentence[NewT, S](empty(), _))

    def fromSentence[NewT <: S#T, S <: ThisSentence[S]](old: S, tokens: IndexedSeq[NewT]) : ThisSentence[NewT]

    def empty[S <: ThisSentence[S]](): S

    implicit def canBuildFrom[NewT <: S#T,S <: ThisSentence[S]]: CanBuildFrom[S, NewT, ThisSentence[NewT]]

    def newCanBuildFrom[NewT <: S#T, S <: ThisSentence[S]]: CanBuildFrom[S, NewT, ThisSentence[NewT]] = new CanBuildFrom[S, NewT, ThisSentence[NewT]] {
      def apply(from: S): mutable.Builder[NewT, ThisSentence[NewT]] = IndexedSeq.newBuilder[NewT] mapResult(fromSentence(from, _))
      def apply(): mutable.Builder[NewT, ThisSentence[NewT]] = newBuilder
    }

    def overwriteTokens[S <: ThisSentence[S]](sentence: S, newTokens: IndexedSeq[S#T]) : S

  }
  object SentenceLike extends GSC2[SentenceLike] {
    def fromSentence[NewT <: S#T, S <: SentenceLike[S]](old: S, tokens: IndexedSeq[NewT]): SentenceLike[NewT] = ???
    def empty[S <: SentenceLike[S]](): S = ???
    def overwriteTokens[S <: SentenceLike[S]](sentence: S, newTokens: IndexedSeq[S#T]): S = ???
    implicit def canBuildFrom[NewT <: S#T, S <: SentenceLike[S]]: CanBuildFrom[S, NewT, SentenceLike[NewT]] = ???
  }


  //GenericSentenceCompanion[ThisSentence[_ <: TokenBound] <: SentenceLike[_ <: TokenLike], TokenBound <: TokenLike]
  /*
  sealed trait GenericSentenceCompanion[ThisSentence[_ <: TokenBound] <: SentenceLike[_ <: TokenLike], TokenBound[_ <: TokenLike] <: TokenLike[_ <: TokenLike]] {
    def newBuilder[T <: TokenBound[T]]: mutable.Builder[T, ThisSentence[T]] = IndexedSeq.newBuilder[T] mapResult (fromSentence(empty(), _))

    def fromSentence[NewT <: TokenBound[NewT],OldT <: TokenBound[OldT]](old: ThisSentence[OldT], tokens: IndexedSeq[NewT]) : ThisSentence[NewT]

    def empty[T <: TokenBound[T]](): ThisSentence[T]

    implicit def canBuildFrom[OldT <: TokenBound[OldT], NewT <: TokenBound[NewT]]: CanBuildFrom[ThisSentence[OldT], NewT, ThisSentence[NewT]]

    def newCanBuildFrom[OldT <: TokenBound[OldT], NewT <: TokenBound[NewT]] :  CanBuildFrom[ThisSentence[OldT], NewT, ThisSentence[NewT]] = new CanBuildFrom[ThisSentence[OldT], NewT, ThisSentence[NewT]] {
      def apply(from: ThisSentence[OldT]): mutable.Builder[NewT, ThisSentence[NewT]] = IndexedSeq.newBuilder[NewT] mapResult(fromSentence(from, _))
      def apply(): mutable.Builder[NewT, ThisSentence[NewT]] = newBuilder
    }

    def overwriteTokens[T <: TokenBound[T]](sentence: ThisSentence[T], newTokens: IndexedSeq[T]) : ThisSentence[T]

  }
  object Sentence extends GenericSentenceCompanion[Sentence, TokenLike] {
    def fromSentence[NewT <: TokenLike[NewT], OldT <: TokenLike[OldT]](old: Sentence[OldT], tokens: IndexedSeq[NewT]): Sentence[NewT] = ???
    def empty[T <: TokenLike[T]](): Sentence[T] = ???
    def overwriteTokens[T <: TokenLike[T]](sentence: Sentence[T], newTokens: IndexedSeq[T]): Sentence[T] = ???
    implicit def canBuildFrom[OldT <: TokenLike[OldT], NewT <: TokenLike[NewT]]: CanBuildFrom[Sentence[OldT], NewT, Sentence[NewT]] = ???
  }
  */

  //object SentenceLike {
  //  implicit def canBuildFrom[NewT <: SentenceLike#T]: CanBuildFrom[SentenceLike, NewT, SentenceLike] = new CanBuildFrom[SentenceLike, NewT, SentenceLike] {
  //    def apply(from: SentenceLike): mutable.Builder[NewT, SentenceLike] = IndexedSeq.newBuilder[NewT] mapResult (from.sentenceCompanion.fromSentence(from, _))
  //    def apply(): mutable.Builder[NewT, SentenceLike] = ???
  //  }

  //}

  val ts = for (i <- 1 to 5) yield Token(i.toString, CharOffsets(i, i + 1))
  val s1 = Sentence(ts)
  println(s1.toString())
  s1.map({ x => TokenWithString(x.word, x.offsets) })
  trait DocumentBasis {
    type S <: SentenceLike[S]
  }

  trait DocumentLike[D <: DocumentLike[D]] extends DocumentBasis with IndexedSeq[DocumentBasis#S] with IndexedSeqLike[DocumentBasis#S, DocumentLike[D]] {
    this: D =>
    val sentences: IndexedSeq[S]
    def length: Int = sentences.length
    def apply(idx: Int): S = sentences(idx)
    override protected[this] def newBuilder: mutable.Builder[DocumentBasis#S, D] = ???
  }
  case class Document[Sentence <: SentenceLike[Sentence]](sentences: IndexedSeq[Sentence]) extends DocumentLike[Document[Sentence]] {
    type S = Sentence
  }
  val ss = for (i <- 1 to 3) yield Sentence(for (j <- i * 3 to (i + 1) * 3) yield Token(j.toString, CharOffsets(j, j + 1)))
  val d1 = Document(ss)

}