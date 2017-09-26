package Examples.Gibbs
import SumProduct._
import breeze.stats.distributions.Gaussian
/**
  * Created by jacoltman on 24/09/2017.
  */
object FittingGaussian extends App {

  val sigma = VariableFactory("sigma")
  val theta = VariableFactory("theta")


  val observations = List(10.0, 9.0, 9.8)
  val variables = List(VariableFactory("x_1"), VariableFactory("x_2"), VariableFactory("x_3"))
  val observation_distributions = variables.map(x => DistributionFactory.gaussian(x, theta, sigma))
  val observations_nodes = (observations zip observation_distributions).map({case (o,d) => NodeFactory.observed(d, o)})

  val sigma_distribution = DistributionFactory(VariableFactory("sigma"), Realization(Map(VariableFactory("sigma")->1.0)))
  val sigma_node = NodeFactory(sigma_distribution, 1.0)
  val theta_distribution = DistributionFactory.gaussian(theta, 0.0, 10.0)
  val theta_node = NodeFactory(theta_distribution, 0.0)

  val graph = GraphFactory.gibbs(List(theta_node, sigma_node) ++ observations_nodes)
  graph.add_edges(theta_node->observations_nodes.head)
  graph.add_edges(theta_node->observations_nodes(1))
  graph.add_edges(theta_node->observations_nodes(2))
  graph.add_edges(sigma_node->observations_nodes.head)
  graph.add_edges(sigma_node->observations_nodes(1))
  graph.add_edges(sigma_node->observations_nodes(2))
  for(x <-1 to 100) {
    graph.run_iteration()
    println(theta_node.current_value)
  }

}
