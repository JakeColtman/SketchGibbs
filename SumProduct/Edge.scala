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

object EdgeFactory extends EdgeMaker {
  def apply(edgeType: EdgeType, from_vertex: Vertex, to_vertex: Vertex): List[Edge] = {
    edgeType match {
      case Directed => List(DirectedEdge(from_vertex, to_vertex, None))
      case DirectedSeparator => List(DirectedSeparatorEdge(from_vertex, to_vertex, None))
      case Undirected => List(DirectedEdge(from_vertex, to_vertex, None), DirectedEdge(to_vertex, from_vertex, None))
      case UndirectedSeparator => List(DirectedSeparatorEdge(from_vertex, to_vertex, None), DirectedSeparatorEdge(to_vertex, from_vertex, None))
    }
  }
}

class EdgeSpec extends FlatSpec with Matchers {
  "Edges " should " be equal if their vertices are the same" in {
    val a = VertexFactory(VariableNode(VariableFactory("a")))
    val b: Vertex = VertexFactory(VariableNode(VariableFactory("b")))
    val edge0 = EdgeFactory(Directed, a, b)
    val edge1 = EdgeFactory(Directed, a, b)
    edge0 should be (edge1)
  }

  "Edges " should " not be equal if their vertices are different " in {
    val a = VertexFactory(VariableNode(VariableFactory("a")))
    val b: Vertex = VertexFactory(VariableNode(VariableFactory("b")))
    val c: Vertex = VertexFactory(VariableNode(VariableFactory("c")))
    val edge0 = EdgeFactory(Directed, a, b)
    val edge1 = EdgeFactory(Directed, a, c)
    edge0 should not be edge1
  }

  "Edges of different types " should " not be equal " in {
    val a = VertexFactory(VariableNode(VariableFactory("a")))
    val b: Vertex = VertexFactory(VariableNode(VariableFactory("b")))
    val edge0 = EdgeFactory(Directed, a, b)
    val edge1 = EdgeFactory(DirectedSeparator, a, b)
    edge0 should not be edge1
  }

  "Undirected edges " should " become 2 directed edges" in {
    val a = VertexFactory(VariableNode(VariableFactory("a")))
    val b: Vertex = VertexFactory(VariableNode(VariableFactory("b")))
    val edges = EdgeFactory(Undirected, a, b)

    edges should contain theSameElementsAs EdgeFactory(Directed, a, b) ++ EdgeFactory(Directed, b, a)
  }

}