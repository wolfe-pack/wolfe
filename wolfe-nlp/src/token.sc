
trait Graph {

  type WhiteContent
  type BlackContent


  trait Node[E <: Edge[_]] {
    def blackParents: Seq[BlackEdge]
    def whiteParents: Seq[WhiteEdge]
    val children:Seq[E] = Seq.empty
  }

  class BlackNode(val blackParents:Seq[BlackEdge], val whiteParents:Seq[WhiteEdge]) extends Node[BlackEdge]
  class WhiteNode(val blackParents:Seq[BlackEdge], val whiteParents:Seq[WhiteEdge]) extends Node[WhiteEdge]

  trait Edge[N <: Node[_]] {
    def to:Node[_]
    def from:N
  }

  class WhiteEdge(val from:WhiteNode,val to:Node[_], content:WhiteContent) extends Edge[WhiteNode]
  class BlackEdge(val from:BlackNode,val to:Node[_], content:BlackContent) extends Edge[BlackNode]

}

trait FactorGraph {
  type WhiteContent
  type BlackContent

  trait Node[E <: Edge[_,_]] {
    def edges:Seq[E]
  }
  trait Edge[N <: Node[_], Content] {
    def node:N
    def factor:Factor
    def content:Content
  }

  class BlackEdge(val node:BlackNode,val factor:Factor, val content:BlackContent) extends Edge[BlackNode,BlackContent]
  class WhiteEdge(val node:WhiteNode,val factor:Factor, val content:WhiteContent) extends Edge[WhiteNode,WhiteContent]
  class BlackNode(val edges:Seq[BlackEdge]) extends Node[BlackEdge]
  class WhiteNode(val edges:Seq[WhiteEdge]) extends Node[WhiteEdge]

  class Factor(val blackEdges:Seq[BlackEdge],val whiteEdges:Seq[WhiteEdge])

}

class MyFactorGraph extends FactorGraph {
  type WhiteContent = Int
  type BlackContent = String
}