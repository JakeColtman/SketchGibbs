package SumProduct

import org.scalatest.{FlatSpec, Matchers}

trait Vertex[NodeType] {
  val content: NodeType
  var incoming_edges: List[Edge[NodeType]] = List()
  var outgoing_edges: List[Edge[NodeType]] = List()

  def ->(vertex: Vertex[NodeType]): List[Edge[NodeType]] = {
    EdgeFactory(Directed, this.content, vertex.content)
  }

  def <->(vertex: Vertex[NodeType]): List[Edge[NodeType]] = {
    EdgeFactory(Undirected, this.content, vertex.content)
  }

}


class VertexSpec extends FlatSpec with Matchers {
  "Vertices " should " be able to created directed edges between themselves" in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")
    a->b should be (EdgeFactory(Directed, a, b))
  }
  "Vertices " should " be able to created undirected edges between themselves" in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")
    a <-> b should contain theSameElementsAs EdgeFactory(Undirected, a, b)
  }
}