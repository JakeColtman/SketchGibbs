package SumProduct
import org.scalatest.{FlatSpec, Matchers}


trait Node {
  def generate_message_to(vertex: Vertex, incoming_messages: Map[Vertex, Message]) : Message
  def variables: List[Variable]
}

class FactorNode(factor: Factor) extends  Node {
  override def generate_message_to(vertex: Vertex, incoming_messages: Map[Vertex, Message]): Message = {
    FactorMessage(factor)
    //marginalize out other variables
  }

  override def variables: List[Variable] = factor.variables
}

case class VariableNode(variable: Variable) extends Node {

  def base_factor() : Factor = {
    val rows = variable.possible_values.map(x => FactorRow(Realization(Map(variable->x)), 1.0))
    RowsFactor(rows)
  }

  def variables: List[Variable] = {
    List(variable)
  }

  def combine_message(messages: List[Message]) : Message = {
    if (messages.isEmpty) return FactorMessage(base_factor())
    val rows = variable.possible_values.map({
      value => {
        val realization = Realization(Map(variable->value))
        val message_value = messages.map(m => m.value_at(realization)).foldLeft(1.0)((x, y) => x * y)
        FactorRow(realization, message_value)
      }
    })
    FactorMessage(RowsFactor(rows))
  }

  override def generate_message_to(vertex: Vertex, incoming_messages: Map[Vertex, Message]): Message = {
    combine_message(incoming_messages.values.toList)
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
    val row0 = FactorRow(Realization(Map(vary->0)), 9.0)
    val row1 = FactorRow(Realization(Map(vary->1)), 10.0)
    val facty = RowsFactor(List(row0, row1))
    val message = FactorMessage(facty)
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
    val row0a = FactorRow(Realization(Map(vary->0)), 9.0)
    val row1a = FactorRow(Realization(Map(vary->1)), 10.0)
    val row0aa = FactorRow(Realization(Map(vary->0)), 2.0)
    val row1aa = FactorRow(Realization(Map(vary->1)), 3.0)
    val factya = RowsFactor(List(row0a, row1a))
    val factyaa = RowsFactor(List(row0aa, row1aa))
    val messagea = FactorMessage(factya)
    val messageaa = FactorMessage(factyaa)

    b.content.generate_message_to(c, Map(a->messagea, aa->messageaa)).variable should be (vary)
    b.content.generate_message_to(c, Map(a->messagea, aa->messageaa)).value_at(Realization(Map(vary->0))) should be (18.0)
    b.content.generate_message_to(c, Map(a->messagea, aa->messageaa)).value_at(Realization(Map(vary->1))) should be (30.0)
  }
}
