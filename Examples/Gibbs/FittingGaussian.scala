package Examples.Gibbs
import SumProduct._
import breeze.stats.distributions.Gaussian
/**
  * Created by jacoltman on 24/09/2017.
  */
object FittingGaussian extends App {
  val observations = List(1.0, 0.9, 1.1)
  val variables = List(VariableFactory("x_1"), VariableFactory("x_2"), VariableFactory("x_3"))
  val point_distributions = (observations zip variables).map({case (x, y) => DistributionFactory(y, Realization(Map(y->x)))})

  val stdev = DistributionFactory(VariableFactory("stdev"), Realization(Map(VariableFactory("stdev")->1.0)))

  def distr_f(realization: Realization): Double = {
    val mean = realization.realization(VariableFactory("mean"))
    val stdev = realization.realization(VariableFactory("stdev"))
    val theta = realization.realization(VariableFactory("theta"))
    Gaussian(mean, stdev).pdf(theta)
  }
  val ff : (Realization => Double) = distr_f

  val distr = DistributionFactory(VariableFactory("theta"), ff)

  def mean_f(realization: Realization): Double = {
    Gaussian(1.0, 10.0).pdf(realization.realization.values.head)
  }

  val mean_ff: (Realization => Double) = mean_f
  val mean = DistributionFactory(VariableFactory("mean"), mean_ff )

  val mean_node = NodeFactory(mean, 1.0)
  val stdev_node = NodeFactory(stdev, 1.0)
  val distr_node = NodeFactory(distr, 1.0)

  val graph = GraphFactory.gibbs(List(mean_node, stdev_node, distr_node))
  graph.add_edges(mean_node->distr_node)
  graph.add_edges(stdev_node->distr_node)

  distr_node.generate_conditional_distribution_wrt(mean_node)
  graph.run_iteration()
}
