package SumProduct
import org.scalatest._

sealed abstract class EdgeType
case object Directed extends EdgeType
case object DirectedSeparator extends EdgeType
case object Undirected extends EdgeType
case object UndirectedSeparator extends EdgeType

trait Edge {
  val from: Vertex
  val to: Vertex
  var message: Option[Message]
}

case class DirectedEdge(from: Vertex, to: Vertex, var message: Option[Message]) extends Edge {
}
case class DirectedSeparatorEdge(from: Vertex, to: Vertex, var message: Option[Message]) extends Edge {
}

trait EdgeMaker {
  def apply(edgeType: EdgeType, from_vertex: Vertex, to_vertex: Vertex): List[Edge]
}

class EdgeFactory extends EdgeMaker {
  def apply(edgeType: EdgeType, from_vertex: Vertex, to_vertex: Vertex): List[Edge] = {
    edgeType match {
      case Directed => List(DirectedEdge(from_vertex, to_vertex, None))
      case DirectedSeparator => List(DirectedSeparatorEdge(from_vertex, to_vertex, None))
      case Undirected => List(DirectedEdge(from_vertex, to_vertex, None), DirectedEdge(to_vertex, from_vertex, None))
      case UndirectedSeparator => List(DirectedSeparatorEdge(from_vertex, to_vertex, None), DirectedSeparatorEdge(to_vertex, from_vertex, None))
    }
  }
}

class EdgeFactorySpec extends FlatSpec with Matchers {
  "Made edges between the same vertexs and without messages " should " equal each other" in {
    val factory = new EdgeFactory()
  }
}