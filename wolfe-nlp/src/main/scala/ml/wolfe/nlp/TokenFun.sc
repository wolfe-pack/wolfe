import ml.wolfe.nlp.{Document2, BasicTokenTest, Sentence2}

BasicTokenTest("Hello World")

val s : Sentence2[BasicTokenTest] = Sentence2.fromString[BasicTokenTest]("Hello World")

s.tokens
s.tokens.length
val d = Document2.fromString[BasicTokenTest,Sentence2[BasicTokenTest]]("Hello World")
d.sentences
d.sentences.size


println("Why not?")
