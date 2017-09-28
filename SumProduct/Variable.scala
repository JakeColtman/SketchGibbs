package SumProduct
import org.scalatest.{FlatSpec, Matchers}


trait Variable {
  val name: String
  val possible_values: List[Double]
  def <= (outcome: Double) : Realization = Realization(Map(VariableFactory(name)->outcome))
  def - (variable: Variable) : Variable = CompositeVariable(this, variable, _ - _ )
  def + (variable: Variable) : Variable = CompositeVariable(this, variable, _ + _ )
  def / (variable: Variable) : Variable = CompositeVariable(this, variable, _ / _ )
  def * (variable: Variable) : Variable = CompositeVariable(this, variable, _ * _ )
}

object VariableFactory {
  def apply(name: String) : Variable = TFVariable(name)
  def apply(names: List[String]) : List[Variable] = names.map(n => VariableFactory(n))

  def apply(value: Double) : Variable = ConstantVariable(value)

  def apply(name: String, size: Int): List[Variable] = {
    (List.fill(size)(name) zip (1 to size)).map({case (n, i) => VariableFactory(n + "_" + i.toString)})
  }
}

case class ConstantVariable(value: Double) extends Variable {
  val name = value.toString
  override val possible_values: List[Double] = List(value)
}

case class TFVariable(name: String) extends Variable{
  val possible_values = List(0.0,1.0)
}

class VariableSpec extends FlatSpec with Matchers {
  "A variable " should "return a list of possible values" in {
    val a = VariableFactory("a")
    a.possible_values.head should be (0.0)
    a.possible_values.size should be (2)
  }
}

