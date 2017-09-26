package SumProduct

import org.scalatest.{FlatSpec, Matchers}

trait Vertex[NodeType] {
  val content: NodeType
  var incoming_edges: List[Edge[NodeType]] = List()
  var outgoing_edges: List[Edge[NodeType]] = List()

  def ->(vertex: Vertex[NodeType]): List[Edge[NodeType]] = {
    EdgeFactory(Directed, this.content, vertex.content)
  }

  def ->(vertices: List[Vertex[NodeType]]): List[Edge[NodeType]] = {
    vertices.flatMap(v => EdgeFactory(Directed, this.content, v.content))
  }

  def <->(vertex: Vertex[NodeType]): List[Edge[NodeType]] = {
    EdgeFactory(Undirected, this.content, vertex.content)
  }

}


class VertexSpec extends FlatSpec with Matchers {
  "Vertices " should " be able to create directed edges between themselves" in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")
    a->b should be (EdgeFactory(Directed, a, b))
  }
  "Vertices " should " be able to create undirected edges between themselves" in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")
    a <-> b should contain theSameElementsAs EdgeFactory(Undirected, a, b)
  }
  "Vertices " should " be able to generate to many other vertices quickly " in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")
    val c = NodeFactory("c")
    a->List(b,c) should contain theSameElementsAs EdgeFactory(Directed, a, b) ++ EdgeFactory(Directed, a, c)
  }
}