

trait Edge {
  def src:Node
  def tgt:Node

}
trait Node {

}

trait Graph {
  type E <: Edge
  type N <: Node

  def nodes:List[N]
  def edges:List[E]

}

trait WeightedEdge extends Edge

trait WeightedGraph {
  type E <: WeightedEdge


}

