package SumProduct
import org.scalatest._

case class CompositeVariable(left_variable: Variable, right_variable: Variable, f: (Double, Double) => Double) extends Variable {
  override val name: String = left_variable.name + " with " + right_variable.name
  override val possible_values: List[Double] = left_variable.possible_values
}

case class ModifiedVariable(variable: Variable, f: Double => Double) extends Variable {
  override val name: String = variable.name
  override val possible_values: List[Double] = variable.possible_values
}

case class Realization(realization: Map[Variable, Double]) {
  def ++(other_realization: Realization): Realization = {
    Realization(other_realization.realization ++ realization)
  }

  def apply(variable: Variable): Double = variable match {
    case tf: TFVariable => realization(tf)
    case cv: ConstantVariable => cv.value
    case comp: CompositeVariable =>
      comp.f(this(comp.left_variable), this(comp.right_variable))
    case comp: ModifiedVariable =>
      comp.f(this(comp.variable))
  }
}

class RealizationSpec extends FlatSpec with Matchers {
  "A realization " should " return the value of a constant variable" in {
    val constant = VariableFactory(1.0)
    Realization(Map())(constant) should be (1.0)
  }

  "A realization " should " be able to handle simple composite variable" in {
    val lhs = VariableFactory(1.0)
    val rhs = VariableFactory(2.0)
    val comp = CompositeVariable(lhs, rhs, _ + _)
    Realization(Map())(comp) should be (3.0)
  }

  "A realization " should " should be able to handle nested composite variable" in {
    val a = VariableFactory(1.0)
    val b = VariableFactory(2.0)
    val lhs = CompositeVariable(a, b, _ + _)

    val rhs = VariableFactory(10.0)
    val comp = CompositeVariable(lhs, rhs, _ + _)
    Realization(Map())(comp) should be (13.0)

    val nested_rhs = CompositeVariable(VariableFactory(9.0), rhs, _ - _)
    val nested_comp = CompositeVariable(lhs, nested_rhs, _ + _)
    Realization(Map())(nested_comp) should be (2.0)
  }

  "A realization " should " should be able to handle TFVariables smoothly" in {
    val a = VariableFactory(1.0)
    val b = VariableFactory(2.0)
    val lhs = CompositeVariable(a, b, _ + _)

    val rhs = VariableFactory(10.0)
    val comp = CompositeVariable(lhs, rhs, _ + _)
    Realization(Map())(comp) should be (13.0)

    val nested_rhs = CompositeVariable(VariableFactory("b"), rhs, _ - _)
    val nested_comp = CompositeVariable(lhs, nested_rhs, _ + _)
    (VariableFactory("b")<=9.0)(nested_comp) should be (2.0)
  }

  "A realization " should " be able to handle single variable modifications " in {
    val a = VariableFactory(2.0)
    val mod_a = ModifiedVariable(a, x => x + 2.0)
    Realization(Map())(mod_a) should be (4.0)
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
    val new_maps = rows.map(row => row.realization.realization).map(mappy => mappy.filterKeys(key => key==variable))
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

  def normalize(factor: Factor) : Factor = {
    val summed_weight = factor.rows.map(fr => fr.value).sum
    val normalized_rows = factor.rows.map(fr => FactorRow(fr.realization, fr.value / summed_weight))
    FactorFactory(normalized_rows)
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
    facty.marginalize(a).marginal_value_at(a<=1) should be (p_true +- 0.01)
    facty.marginalize(b).marginal_value_at(b<=0) should be ((1.0 - q_true) +- 0.01)
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
