package Examples.Gibbs
import SumProduct._

object FittingGaussian extends App {

  val sigma = VariableFactory("sigma")
  val theta = VariableFactory("theta")

  val observations = List(10.0, 9.0, 9.8)
  val variables = VariableFactory("x", 3)
  val observation_distributions = variables.map(x => DistributionFactory.gaussian(x, theta, sigma))
  val observations_nodes = (observations zip observation_distributions).map({case (o,d) => NodeFactory.observed(d, o)})

  val sigma_distribution = DistributionFactory(VariableFactory("sigma"), Realization(Map(VariableFactory("sigma")->1.0)))
  val sigma_node = NodeFactory(sigma_distribution, 1.0)

  val theta_node = Gaussian(theta, 0.0, 10.0)

  val graph = {
    GraphFactory.gibbs(List(theta_node, sigma_node) ++ observations_nodes)
  }
  graph.infer_edges()
  val runner = MeanGibbsGraphRunner(graph, List(theta_node.content))
  runner.run(100, 500)
  println(runner.output(theta_node.content))

}
