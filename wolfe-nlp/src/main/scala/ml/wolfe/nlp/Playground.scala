package ml.wolfe.nlp

/**
 * @author riedel
 */
object Playground extends App {

  import ml.wolfe.term.TermImplicits._

  println(Doubles.Const(1.0) == Doubles.Const(1.0))

//  def parse()
}

//trait Sent {
//  val env:Env
//  def tokens:IndexedSeq[env.Token]
//}
//
//trait Tok {
//  def word:String
//}
//
////case class BasicTok(word:String)
//case class BasicSent[T<:Tok](tokens:IndexedSeq[T]) extends Sent
//case class BasicTok(word:String) extends Tok
//case class BasicDoc[S<:Sent](sentences:IndexedSeq[S]) extends Doc
//
//trait Doc {
//  val env:Env
//  def sentences:IndexedSeq[env.Sentence]
//}
//
//trait Env {
//  type Sentence <: Sent
//  type Token <: Tok
//  type Document <: Doc
//}
//
//object BasicEnv extends Env {
//  type Sentence = BasicSent[Token]
//  type Document = BasicDoc[Sentence]
//  type Token = BasicTok
//}
//
