package SumProduct
import org.scalatest.{FlatSpec, Matchers}


trait Node {
  def generate_message_to(vertex: Vertex, incoming_messages: Map[Vertex, Message]) : Message
  def variables: List[Variable]
}

case class FactorNode(factor: Factor) extends  Node {
  override def generate_message_to(vertex: Vertex, incoming_messages: Map[Vertex, Message]): Message = {
    val combined_factor = incoming_messages.foldLeft(factor)({case (a, (v, m)) => a.mult(m.factor)})
    val marginalized_factor = combined_factor.marginalize(vertex.content.variables.head)
    MessageFactory(marginalized_factor)
  }

  override def variables: List[Variable] = factor.variables
}

case class ObservedVariableNode(variable: Variable, value: Int) extends Node {
  def variables: List[Variable] = {
    List(variable)
  }
  def generate_message_to(vertex: Vertex, incoming_messages: Map[Vertex, Message]): Message = {
    val rows = variable.possible_values.map(x => {
      if (x == value) FactorRow(Realization(Map(variable->x)), 1.0)
      else FactorRow(Realization(Map(variable->x)), 0.0)
    })
    MessageFactory(RowsFactor(rows))
  }
}

case class VariableNode(variable: Variable) extends Node {

  def unnormalized_marginal(messages: List[Message]): Factor = {
    val unnormalized_rows = variable.possible_values.map(x => {
      val value = messages.foldLeft(1.0)((tot, m) => tot * m.value_at(Realization(Map(variable->x))))
      FactorRow(Realization(Map(variable->x)), value)
    } )

    RowsFactor(unnormalized_rows)
  }

  def normalized_marginal(messages: List[Message]): Factor = {
    FactorFactory.normalize(unnormalized_marginal(messages))
  }

  def base_factor() : Factor = {
    val rows = variable.possible_values.map(x => FactorRow(Realization(Map(variable->x)), 1.0))
    RowsFactor(rows)
  }

  def variables: List[Variable] = {
    List(variable)
  }

  def combine_message(messages: List[Message]) : Message = {
    if (messages.isEmpty) return MessageFactory(base_factor())
    val rows = variable.possible_values.map({
      value => {
        val realization = Realization(Map(variable->value))
        val message_value = messages.map(m => m.value_at(realization)).foldLeft(1.0)((x, y) => x * y)
        FactorRow(realization, message_value)
      }
    })
    MessageFactory(RowsFactor(rows))
  }

  override def generate_message_to(vertex: Vertex, incoming_messages: Map[Vertex, Message]): Message = {
    combine_message(incoming_messages.values.toList)
  }
}

class FactorNodeSpec extends FlatSpec with Matchers {
  "A FactorNode receiving a constant factor " should " act like a marginalization" in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")


    val realizations = List((a<=0) ++ (b<=0), (a<=0) ++ (b<=1), (a<=1) ++ (b<=0), (a<=1) ++ (b<=1))
    val values = List(0.2*0.6, 0.2*0.4, 0.8*0.6, 0.8*0.4)
    val facty = FactorFactory(realizations, values)

    val factyNode = FactorNode(facty)
    val factyVertex = VertexFactory(factyNode)

    val vertA = VertexFactory("a")
    val vertB = VertexFactory("b")

    GraphFactory(List(factyVertex, vertA, vertB)).add_edges((vertB<->factyVertex) ++ (factyVertex<->vertA))
    vertB.outgoing_edges.size should be (1)
    factyVertex.incoming_edges.size should be (2)
    vertB.send_message_to(factyVertex)
    factyVertex.incoming_edges.count(e => e.message.isDefined) should be (1)
    factyVertex.send_message_to(vertA)
    val message = vertA.incoming_edges.head.message
    message.get.variable should be (a)
    message.get.value_at(a<=0) should be (0.2)
    message.get.value_at(a<=1) should be (0.8)
  }

}

class VariableNodeSpec extends FlatSpec with Matchers {
  "A VariableNode " should "return a single element list of it's variable when asked" in {
    VariableNode(VariableFactory("a")).variables should be (List(VariableFactory("a")))
  }

  "A VariableNode with only one neighbour " should " produce a multiplication identity for all realizations " in {
    val a = VariableFactory("a")
    val node = VariableNode(a)
    val null_case = node.combine_message(List())
    val values = node.variable.possible_values.map(x => null_case.value_at(Realization(Map(a->x))))
    values should be (a.possible_values.map(x => 1))
  }

  "A VariableNode with two neighbours " should " pass message on unchanged " in {
    val a = VertexFactory("a")
    val b = VertexFactory("b")
    val c = VertexFactory("c")
    val vary = VariableFactory("b")
    GraphFactory(List(a, b,c)).add_edges((a<->b) ++ (b<->c))
    val facty = FactorFactory(List(vary<=0, vary<=1), List(9.0, 10.0))
    val message = MessageFactory(facty)

    b.content.generate_message_to(c, Map(a->message)).variable should be (vary)
    b.content.generate_message_to(c, Map(a->message)).value_at(Realization(Map(vary->0))) should be (9.0)
    b.content.generate_message_to(c, Map(a->message)).value_at(Realization(Map(vary->1))) should be (10.0)
  }

  "A VariableNode with greater than two neighbours " should " take the product at every realization" in {
    val a = VertexFactory("a")
    val aa = VertexFactory("aa")
    val b = VertexFactory("b")
    val c = VertexFactory("c")
    val vary = VariableFactory("b")
    GraphFactory(List(a, b,c)).add_edges((a<->b) ++ (aa<->b) ++ (b<->c))
    val factya = FactorFactory(List(vary<=0, vary<=1), List(9.0, 10.0))
    val factyaa = FactorFactory(List(vary<=0, vary<=1), List(2.0, 3.0))
    val messagea = MessageFactory(factya)
    val messageaa = MessageFactory(factyaa)

    b.content.generate_message_to(c, Map(a->messagea, aa->messageaa)).variable should be (vary)
    b.content.generate_message_to(c, Map(a->messagea, aa->messageaa)).value_at(vary<=0) should be (18.0)
    b.content.generate_message_to(c, Map(a->messagea, aa->messageaa)).value_at(vary<=1) should be (30.0)
  }
}
