package SumProduct
import org.scalatest._

case class Realization(realization: Map[Variable, Int]) {
  def ++(other_realization: Realization): Realization = {
    Realization(other_realization.realization ++ realization)
  }
}

case class FactorRow(realization: Realization, value: Double)

trait Factor {
  def variables : List[Variable]
  def marginal_value_at(realization: Realization): Double
  def value_at(realization: Realization): Double
  def marginalize(variable: Variable) : Factor
  def mult(factor: Factor) : Factor
  def rows: List[FactorRow]
}

case class RowsFactor(rows: List[FactorRow]) extends Factor {

  def variables : List[Variable] = rows.head.realization.realization.keys.toList

  def marginal_value_at(realization: Realization): Double = {
    val new_rows = rows.map(row => FactorRow(Realization(row.realization.realization.filterKeys(key => realization.realization.keys.toList.contains(key))), row.value))
    new_rows.filter(x => x.realization.realization == realization.realization).map(realization => realization.value).sum
  }

  def value_at(realization: Realization): Double = {
    val reduced_realization = Realization(realization.realization.filterKeys(key => variables.contains(key)))
    rows.filter(x => x.realization == reduced_realization).head.value
  }

  def marginalize(variable: Variable) : Factor = {
    val new_maps = rows.map(row => row.realization.realization).map(mappy => mappy.filterKeys(key => key!= variable))
    val new_factor_rows = new_maps.distinct.map(real => FactorRow(Realization(real), marginal_value_at(Realization(real))))
    RowsFactor(new_factor_rows)
  }

  def mult(factor: Factor) : Factor = {
    RowsFactor(rows.map((row) => FactorRow(row.realization, row.value * factor.value_at(row.realization))))
  }

}

case object FactorFactory {
  def apply(factorRows: List[FactorRow]): Factor = {
    RowsFactor(factorRows)
  }
  def apply(realizations: List[Realization], values: List[Double]): Factor = {
    FactorFactory((realizations zip values).map({case(r, d) => FactorRow(r, d)}))
  }
}

class FactorSpec extends FlatSpec with Matchers {
    "A factor " should "accurately record its variables" in {
      val a = VariableFactory("a")
      val b = VariableFactory("b")
      FactorFactory(List((a<=0) ++ (b<=0)), List(1.0)).variables should contain theSameElementsAs List(a, b)
    }

    "A single variable factor " should "act like a lookup" in  {
      val a = VariableFactory("a")
      val facty = FactorFactory(List(a<=0, a<=1), List(0.8, 0.2))
      facty.value_at(a<=0) should be (0.8)
      facty.value_at(a<=1) should be (0.2)
    }

    "Partial realizations " should " return SUM(realizations)" in {
      val a = VariableFactory("a")
      val b = VariableFactory("b")

      val p_true = 0.2
      val q_true = 0.6
      val realizations = List((a<=0) ++ (b<=0), (a<=0) ++ (b<=1), (a<=1) ++ (b<=0), (a<=1) ++ (b<=1))
      val values = List((1.0 - p_true) * (1.0 - q_true), (1.0 - p_true) * q_true, p_true * (1.0 - q_true),  p_true * q_true)
      val facty = FactorFactory(realizations, values)
      facty.marginal_value_at(a<=0) should be (1 - p_true)
    }

  "Indep factor " should " marginalize indep" in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")

    val p_true = 0.2
    val q_true = 0.6

    val realizations = List((a<=0) ++ (b<=0), (a<=0) ++ (b<=1), (a<=1) ++ (b<=0), (a<=1) ++ (b<=1))
    val values = List((1.0 - p_true) * (1.0 - q_true), (1.0 - p_true) * q_true, p_true * (1.0 - q_true),  p_true * q_true)
    val facty = FactorFactory(realizations, values)

    facty.marginalize(b).marginal_value_at(a<=1) should be (p_true +- 0.01)
    facty.marginalize(a).marginal_value_at(b<=0) should be ((1.0 - q_true) +- 0.01)
  }

  "Superset realizations " should " act like set realizations" in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")
    val facty = FactorFactory(List(a<=0, a<=1), List(0.2, 0.8))
    facty.value_at((a<=1) ++ (b<=1)) should be (facty.value_at(a<=1))
    facty.value_at((a<=1) ++ (b<=0)) should be (facty.value_at(a<=1))
    facty.value_at(Realization(Map(a->1, b->0))) should be (0.8)
  }

  "Factors " should "multiply nicely" in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")

    val realizations = List((a<=0) ++ (b<=0), (a<=0) ++ (b<=1), (a<=1) ++ (b<=0), (a<=1) ++ (b<=1))
    val values = List(0.1, 0.2, 0.3, 0.4)
    val facty = FactorFactory(realizations, values)

    val other_facty = FactorFactory(List(b<=0, b<=1), List(0.2, 0.8))

    val multed_factor = facty.mult(other_facty)
    multed_factor.value_at((a<=0) ++ (b<=1)) should be ((0.2 * 0.8) +- 0.01)

  }

}
