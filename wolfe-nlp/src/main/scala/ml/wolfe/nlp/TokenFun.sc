case class CharOffsets(start: Int, end: Int)
object Test{
  case class Token(word: String, offsets: CharOffsets) extends TokenLike[Token] {
    protected def companion: companion[Token] = Token
  }
  object Token extends companion[Token] {
    def overwriteWord(token: Token, newWord: String): Token = token.copy(word = newWord)
  }
  trait TokenLike[T <: TokenLike[T]] {
    this: T =>
    val word: String
    def toPrettyString = word
    val offsets: CharOffsets
    def overwriteWord(newWord: String) : T = companion.overwriteWord(this, newWord)
    protected def companion : companion[T]
  }
  case class TokenE(word: String, offsets: CharOffsets, pos : String) extends TokenLike[TokenE] {
    protected def companion: companion[TokenE] = TokenE
  }
  object TokenE extends companion[TokenE] {
    def overwriteWord(token: TokenE, newWord: String): TokenE = {
      token.copy(word = newWord)
    }
  }
  trait companion[X] {
    def overwriteWord(token: X, newWord: String): X
  }
}

def test[X <: Test.TokenLike[X]](t : X): X = {
  t.overwriteWord("NewWord")
}

val t1 = Test.Token("Hello", CharOffsets(1,2))
println(t1)
t1.overwriteWord("no")
test(t1)
t1.getClass

