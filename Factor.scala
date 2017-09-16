package SumProduct
import org.scalatest._

case class Realization(realization: Map[Variable, Int])

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
    rows.foreach(println)
    factor.variables.foreach(println)
    rows.map((row) => factor.value_at(row.realization)).foreach(println)

    RowsFactor(rows.map((row) => FactorRow(row.realization, row.value * factor.value_at(row.realization))))
  }

}

class FactorSpec extends FlatSpec with Matchers {
    "A factor " should "accurately record its variables" in {
      val a = VariableFactory("a")
      val b = VariableFactory("b")
      val row = FactorRow(Realization(Map(a->0,b->0)), 1.0)
      RowsFactor(List(row)).variables should be (List(a, b))
    }

    "A single variable factor " should "act like a lookup" in  {
      val a = VariableFactory("a")
      val row_false = FactorRow(Realization(Map(a->0)), 0.8)
      val row_true = FactorRow(Realization(Map(a->1)), 0.2)

      val facty = RowsFactor(List(row_false, row_true))
      facty.value_at(Realization(Map(a->0))) should be (0.8)
      facty.value_at(Realization(Map(a->1))) should be (0.2)
    }

    "Partial realizations " should " return SUM(realizations)" in {
      val a = VariableFactory("a")
      val b = VariableFactory("b")

      val p_true = 0.2
      val q_true = 0.6

      val row00 = FactorRow(Realization(Map(a->0,b->0)), (1.0 - p_true) * (1.0 - q_true))
      val row01 = FactorRow(Realization(Map(a->0,b->1)), (1.0 - p_true) * q_true)
      val row10 = FactorRow(Realization(Map(a->1,b->0)), p_true * (1.0 - q_true))
      val row11 = FactorRow(Realization(Map(a->1,b->1)), p_true * q_true)

      val facty = RowsFactor(List(row00, row01, row10, row11))
      facty.marginal_value_at(Realization(Map(a->0))) should be (1 - p_true)
    }

  "Indep factor " should " marginalize indep" in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")

    val p_true = 0.2
    val q_true = 0.6

    val row00 = FactorRow(Realization(Map(a->0,b->0)), (1.0 - p_true) * (1.0 - q_true))
    val row01 = FactorRow(Realization(Map(a->0,b->1)), (1.0 - p_true) * q_true)
    val row10 = FactorRow(Realization(Map(a->1,b->0)), p_true * (1.0 - q_true))
    val row11 = FactorRow(Realization(Map(a->1,b->1)), p_true * q_true)

    val facty = RowsFactor(List(row00, row01, row10, row11))
    facty.marginalize(b).marginal_value_at(Realization(Map(a->1))) should be (p_true +- 0.01)
    facty.marginalize(a).marginal_value_at(Realization(Map(b->0))) should be ((1.0 - q_true) +- 0.01)
  }

  "Superset realizations " should " act like set realizations" in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")
    val row0 = FactorRow(Realization(Map(a->0)), 0.2)
    val row1 = FactorRow(Realization(Map(a->1)), 0.8)
    val facty = RowsFactor(List(row0, row1))
    facty.value_at(Realization(Map(a->1, b->1))) should be (facty.value_at(Realization(Map(a->1))))
    facty.value_at(Realization(Map(a->1, b->0))) should be (facty.value_at(Realization(Map(a->1))))
    facty.value_at(Realization(Map(a->1, b->0))) should be (0.8)
  }

  "Factors " should "multiply nicely" in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")

    val b_true = 0.3

    val row00 = FactorRow(Realization(Map(a->0,b->0)), 0.1)
    val row01 = FactorRow(Realization(Map(a->0,b->1)), 0.2)
    val row10 = FactorRow(Realization(Map(a->1,b->0)), 0.3)
    val row11 = FactorRow(Realization(Map(a->1,b->1)), 0.4)

    val facty = RowsFactor(List(row00, row01, row10, row11))

    val row0 = FactorRow(Realization(Map(b->0)), 0.2)
    val row1 = FactorRow(Realization(Map(b->1)), 0.8)
    val other_facty = RowsFactor(List(row0, row1))

    val multed_factor = facty.mult(other_facty)
    multed_factor.value_at(Realization(Map(a->0,b->1))) should be ((0.2 * 0.8) +- 0.01)

  }

}
