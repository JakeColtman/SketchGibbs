package Examples.Gibbs
import java.util.Calendar
import SumProduct._

object BigBetaBinomial extends App {
  println(Calendar.getInstance.getTime)

  val trxs = VariableFactory("trx", 10000)
  val priors = VariableFactory("prior_p", 10000)
  val prior_distrs = priors.map(x => DistributionFactory.beta(x, 10.0, 20.0))
  val trx_distrs = (trxs zip priors).map({case (t, p) => DistributionFactory.binomial(t, 20, p)})

  val trx_nodes = trx_distrs.map(x => NodeFactory.observed(x, 5.0))
  val prior_nodes = prior_distrs.map(x => NodeFactory(x, 0.3))

  val graph = GraphFactory.gibbs(prior_nodes ++ trx_nodes)
  (prior_nodes zip trx_nodes).foreach({case (p, t) => graph.add_edges(p->t)})
  println(Calendar.getInstance.getTime)
  for(x <-1 to (500 * 1)) {
    graph.run_iteration()
  }
  println(Calendar.getInstance.getTime)

}
