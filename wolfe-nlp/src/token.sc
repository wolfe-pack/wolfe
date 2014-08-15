import ml.wolfe.nlp.Document

def tokenize(d:Document)  = d
def segment(d:Document) = d
val tokenize2 = (d:Document) => d

val pipeline = tokenize2.andThen(segment)

