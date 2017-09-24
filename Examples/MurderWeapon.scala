package Examples
import SumProduct._

/**
  * Created by jacoltman on 17/09/2017.
  */
object MurderWeapon extends  App{
  println("Implementation of the simple model at http://www.mbmlbook.com/MurderMystery_A_model_of_a_murder.html")
  val murderer = VariableFactory("murderer") //is Grey
  val revolver = VariableFactory("revolver") //murder weapon is revolver

  val prior = FactorFactory(List(murderer<=0.0, murderer<=1.0), List(0.7, 0.3))

  val conditional_realizations = List((murderer<=0.0) ++ (revolver<=0.0), (murderer<=0.0) ++ (revolver<=1.0), (murderer<=1.0) ++ (revolver<=0.0), (murderer<=1.0) ++ (revolver<=1.0))
  val condition_factor_value = List(0.8, 0.2, 0.1, 0.9)
  val condition_weapon = FactorFactory(conditional_realizations, condition_factor_value)

  val murderer_vertex = NodeFactory("murderer")
  val revolver_vertex = NodeFactory(VariableFactory("revolver"), 1.0)
  val prior_vertex = NodeFactory(prior)
  val conditional_vertex = NodeFactory(condition_weapon)

  val graph = GraphFactory.sum_product(List(murderer_vertex, revolver_vertex , prior_vertex, conditional_vertex))
  graph.add_edges((murderer_vertex<->prior_vertex) ++ (murderer_vertex<->conditional_vertex) ++ (conditional_vertex<->revolver_vertex))

  revolver_vertex.send_message_to(conditional_vertex)
  conditional_vertex.send_message_to(murderer_vertex)
  println(revolver_vertex.can_send_to(conditional_vertex))
  println(revolver_vertex.generate_message_to(revolver_vertex, Map()))
  graph.run_to_completion()

  val murder_var = murderer_vertex.content.asInstanceOf[VariableNode]
  val marginal = murder_var.normalized_marginal(murderer_vertex.incoming_edges.map(e => e.message.get))
  marginal.rows.foreach(println)
}

object MurderWeaponAndHair extends  App{
  println("Implementation of the simple model at http://www.mbmlbook.com/MurderMystery_A_model_of_a_murder.html")
  val murderer = VariableFactory("murderer") //is Grey
  val revolver = VariableFactory("revolver") //murder weapon is revolver
  val hair = VariableFactory("hair") // found Grey's hair at location of crime

  val prior = FactorFactory(List(murderer<=0.0, murderer<=1.0), List(0.7, 0.3))

  val revolver_conditional_realizations = List((murderer<=0.0) ++ (revolver<=0.0), (murderer<=0.0) ++ (revolver<=1.0), (murderer<=1.0) ++ (revolver<=0.0), (murderer<=1.0) ++ (revolver<=1.0))
  val revolver_condition_factor_value = List(0.8, 0.2, 0.1, 0.9)
  val revolver_condition_murderer = FactorFactory(revolver_conditional_realizations, revolver_condition_factor_value)

  val hair_conditional_realizations = List((murderer<=0.0) ++ (hair<=0.0), (murderer<=0.0) ++ (hair<=1.0), (murderer<=1.0) ++ (hair<=0.0), (murderer<=1.0) ++ (hair<=1.0))
  val hair_conditional_factor_value = List(0.9, 0.1, 0.5, 0.5)
  val hair_conditional_murderer = FactorFactory(hair_conditional_realizations, hair_conditional_factor_value)

  val murderer_vertex = NodeFactory("murderer")
  val revolver_vertex = NodeFactory(VariableFactory("revolver"), 1.0)
  val hair_vertex = NodeFactory(VariableFactory("hair"), 1.0)
  val prior_vertex = NodeFactory(prior)
  val revolver_conditional_vertex = NodeFactory(revolver_condition_murderer)
  val hair_conditional_vertex = NodeFactory(hair_conditional_murderer)

  val graph = GraphFactory.sum_product(List(murderer_vertex, revolver_vertex , hair_vertex, prior_vertex, revolver_conditional_vertex, hair_conditional_vertex))
  graph.add_edges((murderer_vertex<->prior_vertex) ++ (murderer_vertex<->revolver_conditional_vertex) ++ (revolver_conditional_vertex<->revolver_vertex) ++ (murderer_vertex<->hair_conditional_vertex) ++ (hair_conditional_vertex<->hair_vertex))

  graph.run_to_completion()

  val murder_var = murderer_vertex.content.asInstanceOf[VariableNode]
  val marginal = murder_var.normalized_marginal(murderer_vertex.incoming_edges.map(e => e.message.get))
  marginal.rows.foreach(println)
}

