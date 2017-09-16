package SumProduct

trait Vertex {
  val content: Node
  val incoming_edges: List[Edge]
  val outgoing_edges: List[Edge]

  def can_send_to(vertex: Vertex) : Boolean = {
    outgoing_edges.map(e => e.to).contains(vertex) & !incoming_edges.filter(e => e.from != vertex).exists(e => e.message.isEmpty)
  }

  def receive_message(from_vertex: Vertex, message: Option[Message]) : Unit = {
    incoming_edges.filter(e => e.to == from_vertex).head.message = message
  }

  def send_message_to(vertex: Vertex) : Unit = {
    if (!can_send_to(vertex)) return
    val other_messages = incoming_edges.map(message => (message.from, message.message.get)).toMap
    val message = content.generate_message_to(vertex, other_messages)
    outgoing_edges.filter(e => e.to == vertex).head.message = Some(message)
    vertex.receive_message(this, Some(message))
  }
}
