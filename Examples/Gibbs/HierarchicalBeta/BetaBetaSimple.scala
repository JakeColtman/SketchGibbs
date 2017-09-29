package Examples.Gibbs

import java.util.Calendar
import SumProduct._

object HierarchicalBetaBetaSimple extends App {
  import scala.io.Source
  val src = Source.fromFile("data.csv")
  val iter = src.getLines().map(_.split(","))

  var market_names : List[String] = List()
  var property_names: List[String] = List()
  var clicks : List[Int] = List()
  var trx : List[Int] = List()

  for(x<- iter){
    market_names = x(0) :: market_names
    property_names = x(1) :: property_names
    clicks = x(2).toInt :: clicks
    trx = x(3).toInt :: trx

  }

  val prior = VariableFactory("prior")

  val prior_distr = DistributionFactory.beta(prior, 1, 1)
  val prior_node = NodeFactory(prior_distr, 0.02)

  val c0 = VariableFactory("c0")
  val c0_node = Gaussian(c0, 1.0, 100.0)

  val cp = VariableFactory("cp")
  val cp_node = Gaussian(cp, 1.0, 100.0)

  def make_a_distribution(variable: Variable, c_variable: Variable, q_variable: Variable): Distribution = {
    DistributionFactory.beta(variable, c_variable * q_variable, c_variable * (VariableFactory(1.0) - q_variable))
  }

  val markets = VariableFactory(market_names.distinct)
  val market_distrs = markets.map(x => make_a_distribution(x, c0, prior))
  val market_nodes = market_distrs.map(x => NodeFactory(x, 0.02))

  val props = VariableFactory(property_names)
  val prop_distrs = (props zip VariableFactory(market_names)).map( {case (p,m) => make_a_distribution(p, cp, m)})
  val prop_nodes = ((prop_distrs zip clicks) zip trx).map({case ((p, c),t) => NodeFactory(p, (0.001 + t.toDouble) / (0.002 + c.toDouble))})

  val trxs = VariableFactory("trx", props.size)
  val trx_distrs = ((trxs zip props) zip clicks).map({case ((t, p),c) => DistributionFactory.binomial(t, c, p)})
  val trx_nodes = (trx_distrs zip trx).map({case (d,t) => NodeFactory.observed(d, t)})

  val graph = GraphFactory.gibbs(List(prior_node, c0_node, cp_node) ++ market_nodes ++ trx_nodes ++ prop_nodes)

  graph.infer_edges()

  println(Calendar.getInstance.getTime)
  val runner = TraceGibbsGraphRunner(graph, List(prior_node))
  runner.run(500, 1000)
  println(Calendar.getInstance.getTime)


}