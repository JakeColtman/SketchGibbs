package Examples.Gibbs
import SumProduct._
import breeze.stats.distributions.{Gaussian, Beta, Binomial}

/**
  * Created by jacoltman on 26/09/2017.
  */
object SimpleBetaBinomial extends App {

  val prior_p = VariableFactory("prior_p")
  val prior_distr = DistributionFactory.beta(prior_p, 10.0, 20.0)
  val trx = {
    DistributionFactory.binomial(VariableFactory("trx"), 20, prior_p)
  }
  val prior_node = NodeFactory(prior_distr, 0.3)
  val trx_node = NodeFactory.observed(trx, 5.0)
  val graph = GraphFactory.gibbs(List(trx_node, prior_node))
  graph.add_edges(prior_node->trx_node)
  for(x <-1 to 500) {
      graph.run_iteration()
      println(prior_node.current_value)
  }
}
