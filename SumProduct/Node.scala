package SumProduct
import org.scalatest.{FlatSpec, Matchers}


trait SumProductNode {

  this: Vertex[SumProductNode] =>

  def generate_message_to(vertex: SumProductNode, incoming_messages: Map[SumProductNode, Message]) : Message
  def variables: List[Variable]

  def is_complete = !(incoming_edges.exists(e => e.message.isEmpty) || outgoing_edges.exists(e => e.message.isEmpty))

  def emit_messages(): Unit = {
    outgoing_edges.foreach(e => {
      if (can_send_to(e.to)) send_message_to(e.to)
    })
  }

  def can_send_to(vertex: SumProductNode) : Boolean = {
    outgoing_edges.map(e => e.to).contains(vertex) & !incoming_edges.filter(e => e.from != vertex).exists(e => e.message.isEmpty)
  }

  def receive_message(from_vertex: SumProductNode, message: Option[Message]) : Unit = {
    incoming_edges.foreach(e => if (e.from == from_vertex) e.message = message)
  }

  def send_message_to(vertex: SumProductNode) : Unit = {
    if (!can_send_to(vertex)) return
    val other_messages = incoming_edges.filter(m=> m.from != vertex).map(message => (message.from, message.message.get)).toMap
    val message = vertex.generate_message_to(vertex, other_messages)
    outgoing_edges.filter(e => e.to == vertex).head.message = Some(message)
    receive_message(this, Some(message))
  }
}

trait GibbsNode {
  def generate_conditional_distribution(node: GibbsNode) : Distribution
  def update_value(): Unit
  def current_value: Double
  def main_variable: Variable
  def variables: List[Variable]
}

case class DistributionNode(distribution: Distribution, starting_value: Double) extends GibbsNode with Vertex[GibbsNode] {

  var current_value = starting_value
  val content = this
  val main_variable = distribution.main_variable
  override def variables: List[Variable] = distribution.variables

  def generate_conditional_distribution(node: GibbsNode) : Distribution = {
    val other_parent_realizations = incoming_edges.filter(e => e.from != node).map(e =>Realization(Map(e.from.main_variable->e.from.current_value)))
    distribution.condition(other_parent_realizations.reduce(_ ++ _))
  }
  override def update_value(): Unit = {
    val parent_realization : Realization = incoming_edges.map(e => Realization(Map(e.from.main_variable->e.from.current_value))).reduce((x, y) => x ++ y)
    val node_conditional_on_parents: Distribution = distribution.condition(parent_realization)
    val children_conditional_on_parents = outgoing_edges.map(e => e.to.generate_conditional_distribution(this))
    val conditional_distribution = DistributionFactory(main_variable, List(node_conditional_on_parents) ++ children_conditional_on_parents)
    current_value = SliceSampler.sample(conditional_distribution)
  }

}

case class FactorNode(factor: Factor) extends SumProductNode with Vertex[SumProductNode] {
  val content = this
  override def generate_message_to(vertex: SumProductNode, incoming_messages: Map[SumProductNode, Message]): Message = {
    val combined_factor = incoming_messages.foldLeft(factor)({case (a, (v, m)) =>
      m match {
        case factor_message: FactorMessage => a.mult(factor_message.factor)
        case _ => a
      }
    })
    val marginalized_factor = combined_factor.marginalize(vertex.variables.head)
    MessageFactory(marginalized_factor)
  }

  override def variables: List[Variable] = factor.variables
}

case class ObservedVariableNode(variable: Variable, value: Int) extends SumProductNode with Vertex[SumProductNode] {
  val content = this
  def variables: List[Variable] = {
    List(variable)
  }
  def generate_message_to(vertex: SumProductNode, incoming_messages: Map[SumProductNode, Message]): Message = {
    val rows = variable.possible_values.map(x => {
      if (x == value) FactorRow(Realization(Map(variable->x)), 1.0)
      else FactorRow(Realization(Map(variable->x)), 0.0)
    })
    MessageFactory(RowsFactor(rows))
  }
}

case class VariableNode(variable: Variable) extends SumProductNode with Vertex[SumProductNode] {

  val content= this

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

  override def generate_message_to(vertex: SumProductNode, incoming_messages: Map[SumProductNode, Message]): Message = {
    combine_message(incoming_messages.values.toList)
  }
}

case object NodeFactory {
  def apply(variable: Variable) = VariableNode(variable)
  def apply(variable_name: String) = VariableNode(VariableFactory(variable_name))
  def apply(distribution: Distribution, starting_value: Double) = DistributionNode(distribution: Distribution, starting_value)
  def apply(factor: Factor) = FactorNode(factor)
  def apply(variable: Variable, value: Int) = ObservedVariableNode(variable: Variable, value: Int)
}

class FactorNodeSpec extends FlatSpec with Matchers {
  "A FactorNode receiving a constant factor " should " act like a marginalization" in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")

    val realizations = List((a<=0) ++ (b<=0), (a<=0) ++ (b<=1), (a<=1) ++ (b<=0), (a<=1) ++ (b<=1))
    val values = List(0.2*0.6, 0.2*0.4, 0.8*0.6, 0.8*0.4)
    val facty = FactorFactory(realizations, values)

    val factyVertex = NodeFactory(facty)

    val vertA = NodeFactory("a")
    val vertB = NodeFactory("b")

    GraphFactory.sum_product(List(factyVertex, vertA, vertB)).add_edges((vertB<->factyVertex) ++ (factyVertex<->vertA))
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
    val a = NodeFactory("a")
    val b = NodeFactory("b")
    val c = NodeFactory("c")
    val vary = VariableFactory("b")
    GraphFactory.sum_product(List(a, b,c)).add_edges((a<->b) ++ (b<->c))
    val facty = FactorFactory(List(vary<=0, vary<=1), List(9.0, 10.0))
    val message = MessageFactory(facty)

    b.content.generate_message_to(c, Map(a->message)).variable should be (vary)
    b.content.generate_message_to(c, Map(a->message)).value_at(Realization(Map(vary->0))) should be (9.0)
    b.content.generate_message_to(c, Map(a->message)).value_at(Realization(Map(vary->1))) should be (10.0)
  }

  "A VariableNode with greater than two neighbours " should " take the product at every realization" in {
    val a = NodeFactory("a")
    val aa = NodeFactory("aa")
    val b = NodeFactory("b")
    val c = NodeFactory("c")
    val vary = VariableFactory("b")
    GraphFactory.sum_product(List(a, b,c)).add_edges((a<->b) ++ (aa<->b) ++ (b<->c))
    val factya = FactorFactory(List(vary<=0, vary<=1), List(9.0, 10.0))
    val factyaa = FactorFactory(List(vary<=0, vary<=1), List(2.0, 3.0))
    val messagea = MessageFactory(factya)
    val messageaa = MessageFactory(factyaa)

    b.content.generate_message_to(c, Map(a->messagea, aa->messageaa)).variable should be (vary)
    b.content.generate_message_to(c, Map(a->messagea, aa->messageaa)).value_at(vary<=0) should be (18.0)
    b.content.generate_message_to(c, Map(a->messagea, aa->messageaa)).value_at(vary<=1) should be (30.0)
  }
}

class SumProductNodeSpec extends FlatSpec with Matchers {

  "Vertices " should " not be able to send messages to a Vertex not in their outgoing edges " in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")
    a.can_send_to(b) should be (false)
  }
  "Vertices with a single outgoing edge" should " be able to send to it " in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")
    a.outgoing_edges = a.outgoing_edges ++ EdgeFactory(Directed, a, b)
    a.can_send_to(b) should be (true)
    b.can_send_to(a) should be (false)
  }
  "Vertices with multiple outgoing edges" should " not be able to send if hasn't received any messages" in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")
    val c = NodeFactory("c")
    a.outgoing_edges = a.outgoing_edges ++ EdgeFactory(Directed, a, b)
    a.outgoing_edges = a.outgoing_edges ++ EdgeFactory(Directed, a, c)
    a.incoming_edges = a.incoming_edges ++ EdgeFactory(Directed, b, a)
    a.incoming_edges = a.incoming_edges ++ EdgeFactory(Directed, c, a)
    a.can_send_to(b) should be (false)
    a.can_send_to(c) should be (false)
  }
  "Vertices with multiple outgoing edges" should " be to send to a vertex once they've received all other messages" in {
    val a = NodeFactory("a")
    val b = NodeFactory("b")
    val c = NodeFactory("c")
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