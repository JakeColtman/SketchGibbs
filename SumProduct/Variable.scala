package SumProduct
import org.scalatest.{FlatSpec, Matchers}



trait Variable {
  val name: String
  val possible_values: List[Double]
  def <= (outcome: Double) : Realization = Realization(Map(VariableFactory(name)->outcome))
}

object VariableFactory {
  def apply(name: String) : Variable = TFVariable(name)
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

