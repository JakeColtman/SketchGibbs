package Examples.Gibbs

import java.util.Calendar

import SumProduct._

import scala.util.Random

/**
  * Created by jacoltman on 26/09/2017.
  */
object BetaBetaSimple extends App {

  val prior = VariableFactory("prior")

  val prior_distr = DistributionFactory.beta(prior, 1, 1)
  val prior_node = NodeFactory(prior_distr, 0.5)

  val c0 = VariableFactory("c0")
  val c0_distr = DistributionFactory.uniform(c0)
  val c0_node = NodeFactory(c0_distr, 1.0)

  val cp = VariableFactory("cp")
  val cp_distr = DistributionFactory.uniform(cp)
  val cp_node = NodeFactory(cp_distr, 1.0)

  val market_names = List("a", "a", "a", "b", "b")
  val property_names = List("v", "w", "x", "y", "z")
  val clicks = List(10, 10, 10, 10, 10)
  val trx = List(5, 6, 4, 7, 9)
//
//  def make_a_distribution(variable: Variable, c_variable: Variable, q_variable: Variable): Distribution = {
//    DistributionFactory.beta(variable, c_variable - q_variable, c_variable - (VariableFactory(1.0) - q_variable))
//  }
//
//  val markets = VariableFactory(market_names.distinct)
//  val market_distrs = markets.map(x => make_a_distribution(x, c0, prior))
//  val market_nodes = market_distrs.map(x => NodeFactory(x, new Random().nextDouble()))
//
//
//  val props = VariableFactory(property_names)
//  val market_property_lookup = (props zip market_names).toMap
//  market_property_lookup.keys.foreach(println)
//  val prop_distrs = props.map( p => make_a_distribution(p, cp, market_property_lookup(p.name)))
//  val prop_nodes = prop_distrs.map(x => NodeFactory(x, 0.5))
//
//  val trxs = VariableFactory("trx", props.size)
//  val trx_distrs = (trxs zip props).map({case (t, p) => DistributionFactory.binomial(t, 400, p)})
//  val trx_nodes = trx_distrs.map(x => NodeFactory.observed(x, new Random().nextInt(50) + 150))
//
//  val graph = GraphFactory.gibbs(List(prior_node, c0_node, cp_node) ++ market_nodes ++ trx_nodes ++ prop_nodes)
//
//  graph.add_edges(prior_node -> market_nodes)
//  graph.add_edges(c0_node -> market_nodes)
//  graph.add_edges(cp_node -> prop_nodes)
//
//  val market_to_node_lookup = (markets zip market_nodes).toMap
//  prop_nodes.foreach(p => {
//    val mn = market_to_node_lookup(market_property_lookup(p.distribution.variable.name))
//    graph.add_edges(mn->p)
//  })
//
//  graph.add_edges((prop_nodes zip trx_nodes).flatMap({case (p, t) => p->t}))
//
//  println(Calendar.getInstance.getTime)
//  val runner = TraceGibbsGraphRunner(graph, List(prior_node) ++ prop_nodes)
//  runner.run(50, 100)
//  println(Calendar.getInstance.getTime)
//  runner.output(prop_nodes(1)).foreach(println)
//  println(trx_nodes(1).current_value)


}