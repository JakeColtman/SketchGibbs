package Examples.Gibbs
import java.util.Calendar

import SumProduct._
import scala.util.Random
/**
  * Created by jacoltman on 26/09/2017.
  */
object BetaBeta extends App {

  println(Calendar.getInstance.getTime)

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

  def make_a_distribution(variable: Variable, c_variable: Variable, q_variable: Variable): Distribution = {
    DistributionFactory.beta(variable, c_variable - q_variable, c_variable - (VariableFactory(1.0) - q_variable))
  }

  val market_variables = VariableFactory(market_names)


  val n_markets = 200
  val prop_per_market = 50

  val markets = VariableFactory("market", n_markets)
  val market_distrs = markets.map(x => make_a_distribution(x, c0, prior))
  val market_nodes = market_distrs.map(x => NodeFactory(x, new Random().nextDouble()))

  val cms = VariableFactory("cm", n_markets)
  val cm_distrs = cms.map(x => DistributionFactory.uniform(x))
  val cm_nodes = cm_distrs.map(x => NodeFactory(x, new Random().nextDouble()))

  var market_variable_lookup = List.fill(prop_per_market)(markets).flatten
  val cm_lookup = List.fill(prop_per_market)(cms).flatten

  val props = VariableFactory("prop", prop_per_market * n_markets)
  val prop_distrs = (props zip market_variable_lookup).map({case (p, m) => make_a_distribution(p, cp, m)})
  val prop_nodes = prop_distrs.map(x => NodeFactory(x, 0.5))

  val trxs = VariableFactory("trx", prop_per_market * n_markets)
  val trx_distrs = (trxs zip props).map({case (t, p) => DistributionFactory.binomial(t, 400, p)})
  val trx_nodes = trx_distrs.map(x => NodeFactory.observed(x, new Random().nextInt(50) + 150))

  val graph = GraphFactory.gibbs(List(prior_node, c0_node, cp_node) ++ market_nodes ++ trx_nodes ++ prop_nodes)

  graph.add_edges(prior_node -> market_nodes)
  graph.add_edges(c0_node -> market_nodes)
  graph.add_edges(cp_node -> prop_nodes)

  val market_node_lookup = List.fill(prop_per_market)(market_nodes).flatten
  graph.add_edges((market_node_lookup zip prop_nodes).flatMap({case (m, p) => m->p}))

  graph.add_edges((prop_nodes zip trx_nodes).flatMap({case (p, t) => p->t}))

  println(Calendar.getInstance.getTime)
  val runner = MeanGibbsGraphRunner(graph, List(prior_node))
  runner.run(50, 100)
  println(Calendar.getInstance.getTime)


}