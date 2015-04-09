//package ml.wolfe.nlp.Data
//
//import ml.wolfe.nlp.generics.{GenericDocumentCompanion, GenericSentenceCompanion}
//import ml.wolfe.nlp.structures._
//
//import scala.collection.generic.CanBuildFrom
//import scala.collection.immutable.IndexedSeq
//import scala.collection.{IndexedSeqLike, mutable}
//import scala.language.{higherKinds, implicitConversions}
//
//
//object Data2 {
//
//
//  trait SentenceWithStringLike[T <: TokenLike] extends SentenceLike[T] with IndexedSeq[T] with IndexedSeqLike[T,SentenceWithStringLike[T]]{
//    override protected def newBuilder: mutable.Builder[T, SentenceWithStringLike[T]] = SentenceWithStringLike.newBuilder
//  }
//  object SentenceWithStringLike extends GenericSentenceCompanion[SentenceWithStringLike, TokenLike] {
//    implicit def canBuildFrom[OldT <: TokenLike, NewT <: TokenLike]: CanBuildFrom[SentenceWithStringLike[OldT], NewT, SentenceWithStringLike[NewT]] = newCanBuildFrom
//    def empty(): SentenceWithStringLike[TokenLike] = new SentenceWithString[TokenLike](IndexedSeq.empty)
//    def fromSentence[NewT <: TokenLike](old: SentenceWithStringLike[_ <: TokenLike], tokens: IndexedSeq[NewT]): SentenceWithStringLike[NewT] = new SentenceWithString[NewT](tokens)
//  }
//  case class SentenceWithString[T <: TokenLike](tokens: IndexedSeq[T]) extends SentenceWithStringLike[T]
//
//  trait SentenceWithTreesLike[T <: TokenLike] extends SentenceLike[T] with IndexedSeq[T] with IndexedSeqLike[T,SentenceWithTreesLike[T]]{
//    val trees: String
//    override protected def newBuilder: mutable.Builder[T, SentenceWithTreesLike[T]] = SentenceWithTreesLike.newBuilder
//  }
//
//  object SentenceWithTreesLike extends GenericSentenceCompanion[SentenceWithTreesLike, TokenLike] {
//    implicit def canBuildFrom[OldT <: TokenLike, NewT <: TokenLike]: CanBuildFrom[SentenceWithTreesLike[OldT], NewT, SentenceWithTreesLike[NewT]] = newCanBuildFrom
//    def empty(): SentenceWithTreesLike[TokenLike] = new SentenceWithTreesLike[TokenLike] {val trees : String                = ""
//                                                                                          val tokens: IndexedSeq[TokenLike] = IndexedSeq.empty
//    }
//    def fromSentence[NewT <: TokenLike](old: SentenceWithTreesLike[_ <: TokenLike], tokens: IndexedSeq[NewT]): SentenceWithTreesLike[NewT] = new SentenceWithTrees[NewT](tokens, old.trees)
//  }
//
//  case class SentenceWithTrees[T <: TokenLike](tokens: IndexedSeq[T], trees: String) extends SentenceWithTreesLike[T]
//
//  trait DocumentWithTreesLike[S <: SentenceLike[_ <: TokenLike]] extends DocumentLike[S] with IndexedSeq[S] with IndexedSeqLike[S, DocumentWithTreesLike[S]] {
//    val trees : String
//    override protected def newBuilder: mutable.Builder[S, DocumentWithTreesLike[S]] = DocumentWithTreesLike.newBuilder
//  }
//
//  object DocumentWithTreesLike extends GenericDocumentCompanion[DocumentWithTreesLike, SentenceLike, TokenLike] {
//
//    implicit def canBuildFrom[OldS <: SentenceLike[_ <: TokenLike], NewS <: SentenceLike[_ <: TokenLike]]: CanBuildFrom[DocumentWithTreesLike[OldS], NewS, DocumentWithTreesLike[NewS]] = newCanBuildFrom
//    def empty(): DocumentWithTreesLike[SentenceLike[TokenLike]] = new DocumentWithTrees("", "", IndexedSeq.empty)
//    def fromDocument[NewS <: SentenceLike[_ <: TokenLike]](old: DocumentWithTreesLike[_ <: SentenceLike[_ <: TokenLike]], sentences: IndexedSeq[NewS]): DocumentWithTreesLike[NewS] = new DocumentWithTrees[NewS](old.source, old.trees, sentences)
//  }
//
//  case class DocumentWithTrees[S <: SentenceLike[_ <: TokenLike]](source: String, trees: String, sentences: IndexedSeq[S]) extends DocumentWithTreesLike[S]
//
//
//  def main(args: Array[String]): Unit = {
//    case class TokenWithPOS(word: String, offsets: CharOffsets, posTag: POSTag) extends TokenLike with TokenWithPOSLike
//
//    // Sentence generation
//    val t1 = for (i <- 1 to 5) yield Token(i.toString, CharOffsets(i-1 + (i-1)*2,i + 2*(i-1)))
//    val s1 = Sentence(t1)
//
//    println(t1)
//    println(s1)
//
//    // Document generation
//    val s3 = for (i <- 1 to 3) yield Sentence(for (j <- i to i +2 ) yield Token(j.toString, CharOffsets(j-1 + (j-1)*2, j + 2*(j-1))))
//
//    val d1 = Document("",s3)
//
//    println(s3)
//    println(d1)
//
//
//    // Extended Sentence generation
//
//    val s1StringLike : SentenceWithString[Token] = SentenceWithString(t1)
//    // This needs a cbf: SentenceWithString[Token], TokenWithOffset, SentenceWithString[TokenWithOffset]
//
//    val s2StringLike = s1StringLike.map({x => TokenWithPOS(x.word, x.offsets, POSTag("Number"))})
//    val s2StringLike3 = s1StringLike.map({x => TokenWithPOS(x.word, x.offsets, POSTag("Number"))})(SentenceWithStringLike.canBuildFrom)
//    assert(s2StringLike == s2StringLike3)
//
//    println(s1StringLike)
//    println(s2StringLike)
//    //println(s2StringLike2)
//
//
//    // Can I change the type of the Tokens in a Document?
//    val d2 = d1 map {s => s map {t => TokenWithPOS(t.word, t.offsets, POSTag("Number"))}}
//
//    println("d2:  " + d2)
//
//    // Can I change the type of Sentences in a Document?
//    val d3 = d1 map {s => SentenceWithString(s.tokens)}
//
//    println("d3:  " + d3)
//
//    // Can I change both at once?
//    val d4 = d1 map {s => SentenceWithString(s map {t => TokenWithPOS(t.word, t.offsets, POSTag("Number"))})}
//
//    println("d4:  " + d4)
//
//    // Can I do both at once with even more modern constructors?
//    val d42 = d1 map {s => SentenceWithTrees(s map {t => TokenWithPOS(t.word, t.offsets, POSTag("Number"))},"A tree")}
//
//    println("d42: " + d42)
//
//    // Can I change the top level Document?
//
//    val d5 = DocumentWithTrees(d1.source, "Trees!", d1.sentences)
//
//    println("d5:  " + d5)
//
//    // Given an extended DocumentType, can I modify the underlying structures?
//
//    val d52 = d5 map {s => SentenceWithString(s map {t => TokenWithPOS(t.word, t.offsets, POSTag("Number"))})}
//
//    println("d52: " + d52)
//
//    val d6 = RichDocument("Hello World, I am Ingolf")
//
//    println(d6)
//
//    val d7 = RichDocument(Seq(Seq("Hello", "World!"), Seq("I", "am", "Ingolf.")))
//
//    println(d7)
//    println(d7.toText)
//  }
//}
//
