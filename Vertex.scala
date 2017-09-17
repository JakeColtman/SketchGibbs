package SumProduct

import org.scalatest.{FlatSpec, Matchers}

trait Vertex {
  val content: Node
  var incoming_edges: List[Edge]
  var outgoing_edges: List[Edge]

  def can_send_to(vertex: Vertex) : Boolean = {

    outgoing_edges.map(e => e.to).contains(vertex) & !incoming_edges.filter(e => e.from != vertex).exists(e => e.message.isEmpty)
  }

  def receive_message(from_vertex: Vertex, message: Option[Message]) : Unit = {
    incoming_edges.foreach(e => if (e.from == from_vertex) e.message = message)
  }

  def send_message_to(vertex: Vertex) : Unit = {
    if (!can_send_to(vertex)) return
    val other_messages = incoming_edges.filter(m=> m.from != vertex).map(message => (message.from, message.message.get)).toMap
    val message = content.generate_message_to(vertex, other_messages)
    outgoing_edges.filter(e => e.to == vertex).head.message = Some(message)
    vertex.receive_message(this, Some(message))
  }

  def ->(vertex: Vertex): List[Edge] = {
    EdgeFactory(Directed, this, vertex)
  }

  def <->(vertex: Vertex): List[Edge] = {
    EdgeFactory(Undirected, this, vertex)
  }

}

case class NodeVertex(node: Node) extends Vertex {
  val content = node
  override var incoming_edges: List[Edge] = List()
  override var outgoing_edges: List[Edge] = List()
}

case object VertexFactory {
  def apply(node: Node): Vertex = {
    NodeVertex(node)
  }
  def apply(variable_name: String): Vertex = {
    VertexFactory(VariableNode(VariableFactory(variable_name)))
  }
}


class VertexSpec extends FlatSpec with Matchers {
  "Vertices " should " be able to created directed edges between themselves" in {
    val a = VertexFactory("a")
    val b = VertexFactory("b")
    a->b should be (EdgeFactory(Directed, a, b))
  }
  "Vertices " should " be able to created undirected edges between themselves" in {
    val a = VertexFactory("a")
    val b = VertexFactory("b")
    a<->b should contain theSameElementsAs EdgeFactory(Undirected, a, b)
  }
  "Vertices " should " not be able to send messages to a Vertex not in their outgoing edges " in {
    val a = VertexFactory("a")
    val b = VertexFactory("b")
    a.can_send_to(b) should be (false)
  }
  "Vertices with a single outgoing edge" should " be able to send to it " in {
    val a = VertexFactory("a")
    val b = VertexFactory("b")
    a.outgoing_edges = a.outgoing_edges ++ EdgeFactory(Directed, a, b)
    a.can_send_to(b) should be (true)
    b.can_send_to(a) should be (false)
  }
  "Vertices with multiple outgoing edges" should " not be able to send if hasn't recieved any messages" in {
    val a = VertexFactory("a")
    val b = VertexFactory("b")
    val c = VertexFactory("c")
    a.outgoing_edges = a.outgoing_edges ++ EdgeFactory(Directed, a, b)
    a.outgoing_edges = a.outgoing_edges ++ EdgeFactory(Directed, a, c)
    a.incoming_edges = a.incoming_edges ++ EdgeFactory(Directed, b, a)
    a.incoming_edges = a.incoming_edges ++ EdgeFactory(Directed, c, a)
    a.can_send_to(b) should be (false)
    a.can_send_to(c) should be (false)
  }
  "Vertices with multiple outgoing edges" should " be to send to a vertex once they've received all other messages" in {
    val a = VertexFactory("a")
    val b = VertexFactory("b")
    val c = VertexFactory("c")
    a.outgoing_edges = a.outgoing_edges ++ EdgeFactory(Directed, a, b)
    a.outgoing_edges = a.outgoing_edges ++ EdgeFactory(Directed, a, c)
    a.incoming_edges = a.incoming_edges ++ EdgeFactory(Directed, b, a)
    a.incoming_edges = a.incoming_edges ++ EdgeFactory(Directed, c, a)
    a.can_send_to(b) should be (false)
    a.can_send_to(c) should be (false)
    a.receive_message(b, Some(MockMessage()))
    a.can_send_to(b) should be (false)
    a.can_send_to(c) should be (true)
    a.receive_message(c, Some(MockMessage()))
    a.can_send_to(b) should be (true)
    a.can_send_to(c) should be (true)

  }

}