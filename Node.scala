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
    FactorMessage(base_factor())
//    val realizations = variable.possible_values.map(x => Realization(Map(variable->x)))
//
//    println(messages.size)
//    FactorMessage(RowsFactor(rows))
    //val factor_for_message = messages.foldLeft(base_factor())((x, y) => x.mult(y.fa//ctor))
    //FactorMessage(factor_for_message)
  }

  override def generate_message_to(vertex: Vertex, incoming_messages: Map[Vertex, Message]): Message = {
    FactorMessage(base_factor())
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
}
