package SumProduct

import org.scalatest.{FlatSpec, Matchers}

trait Graph {
  val vertices: List[Vertex]
  def add_edges(edges: List[Edge]) : Graph
  def is_complete: Boolean
  def run_to_completion(): Unit
}

case class BaseGraph(vertices: List[Vertex]) extends Graph {

  def edge_exists(edge: Edge): Boolean = {
    vertices.exists(v => v.incoming_edges.contains(edge) | v.outgoing_edges.contains(edge))
  }

  def add_edges(edges: List[Edge]) : Graph = {
    edges.foreach(edge => add_edge(edge))
    this
  }

  def add_edge(edge: Edge) : Unit = {
    vertices.foreach(v => if (v == edge.from) v.outgoing_edges = v.outgoing_edges ++ List(edge))
    vertices.foreach(v => if (v == edge.to) v.incoming_edges = v.incoming_edges ++ List(edge))
  }

  override def is_complete: Boolean = {
    !vertices.exists(v => v.is_complete)
  }

  override def run_to_completion(): Unit = {
    while (!is_complete){
      vertices.foreach(v => v.emit_messages())
    }
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
  "A graph " should "be able to coordinate adding edges" in {
    val a = VertexFactory("a")
    val b = VertexFactory("b")

    val graph = GraphFactory(List(a, b))
    val edge = a->b
    graph.add_edges(edge)
    a.outgoing_edges.isEmpty should be (false)
    b.outgoing_edges.isEmpty should be (true)
    b.incoming_edges.isEmpty should be (false)
    a.outgoing_edges should be (a->b)
    b.incoming_edges should be (a->b)

    graph.add_edges(b->a)
    b.outgoing_edges should be (b->a)
  }

}