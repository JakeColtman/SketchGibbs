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
    List()
  }

  def combine_message(messages: List[Message]) : Message = {
    val rows = variable.possible_values.map(x => FactorRow(Realization(Map(variable->x)), 1.0))
    println(messages.size)
    FactorMessage(RowsFactor(rows))
    //val factor_for_message = messages.foldLeft(base_factor())((x, y) => x.mult(y.fa//ctor))
    //FactorMessage(factor_for_message)
  }

  override def generate_message_to(vertex: Vertex, incoming_messages: Map[Vertex, Message]): Message = {
    FactorMessage(base_factor())
  }
}

class NodeSpec extends FlatSpec with Matchers {

}
