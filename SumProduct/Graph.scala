package SumProduct

import org.scalatest.{FlatSpec, Matchers}

trait Graph[NodeType] {

  val vertices : List[Vertex[NodeType]]

  def edge_exists(edge: Edge[NodeType]): Boolean = {
    vertices.exists(v => v.incoming_edges.contains(edge) | v.outgoing_edges.contains(edge))
  }

  def add_edges(edges: List[Edge[NodeType]]) : Graph[NodeType] = {
    edges.foreach(edge => add_edge(edge))
    this
  }

  def add_edge(edge: Edge[NodeType]) : Unit = {
    vertices.foreach(v => if (v == edge.from) v.outgoing_edges = v.outgoing_edges ++ List(edge))
    vertices.foreach(v => if (v == edge.to) v.incoming_edges = v.incoming_edges ++ List(edge))
  }
}

trait GibbsGraph {
  this : Graph[GibbsNode] =>
  def run_iteration(): GibbsGraph
}

case class BaseGibbsGraph(vertices: List[Vertex[GibbsNode]]) extends GibbsGraph with Graph[GibbsNode] {
  override def run_iteration(): GibbsGraph = {
    vertices.foreach(v => v.content.update_value())
    this
  }
}

trait SumProductGraph {
  this : Graph[SumProductNode] =>
  def is_complete: Boolean
  def run_to_completion(): Unit
}

case class BaseSumProductGraph(vertices: List[Vertex[SumProductNode]]) extends SumProductGraph with Graph[SumProductNode] {
    override def is_complete: Boolean = {
      !vertices.exists(v => !v.content.is_complete)
    }

    override def run_to_completion(): Unit = {
      while (!is_complete){
        vertices.foreach(v => v.content.emit_messages())
      }
    }
}

case object GraphFactory{
  def sum_product(vertices: List[Vertex[SumProductNode]]) = BaseSumProductGraph(vertices)
  def gibbs(vertices: List[Vertex[GibbsNode]]) = BaseGibbsGraph(vertices)
}

class GraphSpec extends FlatSpec with Matchers {
  "A graph " should "return a list of its vertices" in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")

    val graph = GraphFactory.sum_product(List(a, b))
    graph.vertices should contain theSameElementsAs List(a, b)
  }
  "A graph " should "be able to coordinate adding edges" in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")

    val graph = GraphFactory.sum_product(List(a, b))
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