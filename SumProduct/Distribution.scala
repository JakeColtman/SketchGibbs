package SumProduct

import breeze.stats.distributions.{Gaussian, Beta, Binomial}
import org.scalatest.{FlatSpec, Matchers}

trait Distribution {
  def f: (Realization => Double)
  def value_at(realization: Realization) : Double
  def value_at(value: Double) : Double
  def variable: Variable
  def condition(other_variables: Realization) : Distribution
  def sample_at(realization: Realization) : Double
  def other_variables: List[Variable]
}

case class PointDistribution(variable: Variable, point_realization: Realization) extends Distribution {
  override def f: (Realization) => Double = value_at

  override def value_at(realization: Realization): Double = {
    if(!point_realization.realization.exists({case (k, v) => realization.realization(k) == v})) 0.0
    else -Double.MinValue
  }
  override def value_at(value: Double): Double = value_at(Realization(Map(variable->value)))
  override def condition(other_variables: Realization): Distribution = this

  override def sample_at(realization: Realization): Double = point_realization.realization(variable)

  override def other_variables: List[Variable] = List()
}

case class FunctionDistribution(variable: Variable, f: (Realization => Double), other_variables: List[Variable]) extends Distribution {
  override def value_at(realization: Realization): Double = {
    try {
      f(realization)
    }
    catch {
      case _: java.lang.IllegalArgumentException => Double.MinValue
    }
  }

  override def condition(other_variable_realization: Realization): Distribution = {
    def new_f(remaining_variables: Realization) = {
      f(remaining_variables ++ other_variable_realization)
    }
    FunctionDistribution(variable, new_f, List(variable))
  }

  override def value_at(value: Double): Double = value_at(Realization(Map(variable->value)))

  override def sample_at(realization: Realization): Double = LogSliceSampler(this, realization(variable)).draw
}

case object DistributionFactory {
  def apply(variable: Variable, f: (Realization => Double)): Distribution = {
    FunctionDistribution(variable, f, List(variable))
  }
  def apply(variable: Variable, true_realization: Realization): Distribution = {
    PointDistribution(variable, true_realization)
  }
  def apply(variable: Variable, distributions : List[Distribution]) : Distribution = {
    def new_f(realization: Realization) : Double = {
      distributions.map(f => f.f(realization)).foldLeft(1.0)(_ + _)
    }
    FunctionDistribution(variable, new_f, List(variable))
  }
  def uniform(variable: Variable): Distribution = FunctionDistribution(variable, x => 0.0, List(variable))
  def gaussian(variable: Variable, mean: Variable, st_dev: Variable) : Distribution = {
    def gaussian_f(realization: Realization): Double = {
      val mean_val = realization(mean)
      val stdev_val = realization(st_dev)
      val theta_val = realization(variable)
      scala.math.log(breeze.stats.distributions.Gaussian(mean_val, stdev_val).pdf(theta_val))
    }
    FunctionDistribution(variable, x => gaussian_f(x), List(mean, st_dev))
  }
  def gaussian(variable: Variable, mean: Double, sigma: Double) : Distribution = {
    def gaussian_f(realization: Realization): Double = {
      val theta = realization(variable)
      scala.math.log(breeze.stats.distributions.Gaussian(mean, sigma).pdf(theta))
    }
    FunctionDistribution(variable, x => gaussian_f(x), List(variable))
  }
  def beta(variable: Variable, alpha: Variable, beta: Variable) : Distribution = {
    def beta_f(realization: Realization): Double = {
      val alpha_val = realization(alpha)
      val beta_val = realization(beta)
      var theta_val = realization(variable)
      if(theta_val == 0.0){theta_val = 0.001}
      if(theta_val == 1.0){theta_val = 0.999}
      scala.math.log(new breeze.stats.distributions.Beta(alpha_val, beta_val).pdf(theta_val))
    }
    FunctionDistribution(variable, x => beta_f(x), List(alpha, beta))
  }
  def beta(variable: Variable, alpha: Double, beta: Double) : Distribution = {
    def beta_f(realization: Realization): Double = {
      val theta_val = realization(variable)
      scala.math.log(new breeze.stats.distributions.Beta(alpha, beta).pdf(theta_val))
    }
    FunctionDistribution(variable, x => beta_f(x), List())
  }
  def binomial(variable: Variable, n: Int, p: Variable) : Distribution = {
    def binomial_f(realization: Realization): Double = {
      val theta_val = realization(variable)
      val p_val = realization(p)
      scala.math.log(breeze.stats.distributions.Binomial(n, p_val).probabilityOf(theta_val.toInt))
    }
    FunctionDistribution(variable, x => binomial_f(x), List(p))
  }
}

class DistributionSpec extends FlatSpec with Matchers {
  "A function distribution " should " be able to correctly interact with its function " in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")

    def f(realization: Realization) : Double = {
      realization(a) * realization(b)
    }
    val functy : Realization => Double = f
    val distry = DistributionFactory(VariableFactory("a"), functy)
    distry.value_at((a<=0.0) ++ (b<=3.0)) should be (0.0 * 3.0)
    distry.value_at((a<=10.0) ++ (b<=3.0)) should be (10.0 * 3.0)
    distry.value_at((a<=10.0) ++ (b<=30.0)) should be (10.0 * 30.0)
  }

  "A function distribution " should " be able to condition out variables " in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")

    def f(realization: Realization) : Double = {
      realization(a) * realization(b)
    }
    val functy : Realization => Double = f
    val distry = DistributionFactory(VariableFactory("a"), functy)
    val conditioned_distry = distry.condition(b<=3.0)
    conditioned_distry.value_at(a<=0.0) should be (0.0 * 3.0)
    conditioned_distry.value_at(a<=10.0) should be (10.0 * 3.0)
  }
}

