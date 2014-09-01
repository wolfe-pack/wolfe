import ml.wolfe.FactorGraph.Factor

class S {
  final class T {
    type Edges = ml.wolfe.FactorGraph.Edge
  }

  type Edges = Seq[T#Edges]

  val edges:Edges = null

}

val s = new S
s.edges

