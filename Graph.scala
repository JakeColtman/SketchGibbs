package SumProduct

import org.scalatest.{FlatSpec, Matchers}

trait Graph {
  val vertices: List[Vertex]
  def add_edges(edges: List[Edge]) : Unit
}

case class BaseGraph(vertices: List[Vertex]) extends Graph {

  def edge_exists(edge: Edge): Boolean = {
    vertices.exists(v => v.incoming_edges.contains(edge) | v.outgoing_edges.contains(edge))
  }

  def add_edges(edges: List[Edge]) : Unit = {
    edges.foreach(edge => add_edge(edge))
  }

  def add_edge(edge: Edge) : Unit = {
    vertices.foreach(v => if (v == edge.from) v.outgoing_edges = v.outgoing_edges ++ List(edge))
    vertices.foreach(v => if (v == edge.to) v.incoming_edges = v.incoming_edges ++ List(edge))
  }

}

case object GraphFactory{
  def apply(vertices: List[Vertex]) = BaseGraph(vertices)
}


class GraphSpec extends FlatSpec with Matchers {
  "A graph " should "return a list of its vertices" in {
    val a = VertexFactory("a")
    val b = VertexFactory("b")

    val graph = GraphFactory(List(a, b))
    graph.vertices should contain theSameElementsAs List(a, b)
  }
}