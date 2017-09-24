package SumProduct

trait Distribution {
  def f: (Realization => Double)
  def value_at(realization: Realization) : Double
  def variables: List[Variable]
  def main_variable: Variable
  def sample_at(realization: Realization): Double
  def condition(other_variables: Realization) : Distribution
}

case class FunctionDistribution(main_variable: Variable, variables: List[Variable], f: (Realization => Double)) extends Distribution {
  override def value_at(realization: Realization): Double = f(realization)

  override def condition(other_variables: Realization): Distribution = {
    def new_f(remaining_variables: Realization) = {
      f(remaining_variables ++ other_variables)
    }
    val remaining_variables = this.variables.filter(x => !other_variables.realization.keys.toList.contains(x))
    FunctionDistribution(main_variable, remaining_variables, new_f)
  }

  override def sample_at(realization: Realization): Double = 0.0
}

case object DistributionFactory {
  def apply(main_variable: Variable, variables: List[Variable], f: (Realization => Double)): Distribution = {
    FunctionDistribution(main_variable, variables, f)
  }
  def apply(main_variable: Variable, variable: Variable, fs : List[(Realization => Double)]) : Distribution = {
    def new_f(realization: Realization) : Double = {
      fs.map(f => f(realization)).foldLeft(1.0)(_ * _)
    }
    FunctionDistribution(main_variable, List(variable), new_f)
  }
  def apply(main_variable: Variable, distributions : List[Distribution]) : Distribution = {
    DistributionFactory(main_variable, main_variable, distributions.map(x => x.f))
  }
  def apply(main_variable: Variable, true_realization: Realization): Distribution = {
    def f(realization: Realization): Double = {
      val matched = !true_realization.realization.exists({case (key, value) => realization.realization.getOrElse(key, -1000) != value})
      if (matched) 1.0 else 0.0
    }
    FunctionDistribution(main_variable, List(main_variable), f)
  }


}