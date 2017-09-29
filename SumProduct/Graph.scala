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

  def infer_edges(): Unit
}

trait GibbsGraph {
  this : Graph[GibbsNode] =>
  def run_iteration(): GibbsGraph
}

case class BaseGibbsGraph(vertices: List[Vertex[GibbsNode]]) extends GibbsGraph with Graph[GibbsNode] {
  override def run_iteration(): GibbsGraph = {
    vertices.foreach(v => {
      v.content.update_value()
    })
    this
  }

  override def infer_edges(): Unit = {
    val vertex_lookup = vertices.map(v => (v.content.variable, v)).toMap
    for(v<-vertices){
      val edges_to_vertex = v.content.other_variables.flatMap(o_v => o_v.contained_variables).flatMap(o_v => {
        vertex_lookup(o_v)->v
      })
      add_edges(edges_to_vertex.distinct)
    }
  }
}

trait GibbsGraphRunner {
  def run(n_burn: Int, n_sample: Int)
}

case class MeanGibbsGraphRunner(graph: GibbsGraph, nodes: List[GibbsNode]) extends GibbsGraphRunner {
  var output : Map[GibbsNode, Double] = nodes.map(x => (x, 0.0)).toMap
  var counter = 0
  def record_step() = {
    graph.run_iteration()
    for(n<-nodes){
      counter = counter + 1
      output = output ++ Map(n ->((output(n) * (counter - 1) + n.current_value) / counter))
    }
  }
  def run(n_burn: Int, n_sample: Int) = {
    for(_<-1 to n_burn){graph.run_iteration()}
    for(_<-1 to n_sample){record_step()}
  }
}

case class TraceGibbsGraphRunner(graph: GibbsGraph, nodes: List[GibbsNode]) extends GibbsGraphRunner {
  var output : Map[GibbsNode, List[Double]] = nodes.map(x => (x, List())).toMap
  var counter = 0
  def record_step() = {
    graph.run_iteration()
    for(n<-nodes){
      output = output ++ Map(n -> (n.current_value :: output(n)))
    }
  }
  def run(n_burn: Int, n_sample: Int) = {
    for(x<-1 to n_burn){
      println(x)
      graph.run_iteration()
    }
    for(x<-1 to n_sample)
    {
      println(x)
      record_step()
    }
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

    override def infer_edges(): Unit = {}
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