package SumProduct
import org.scalatest.{FlatSpec, Matchers}

trait IVariable {
  val name: String
  val p: Double
  val possible_values: List[Int]
}

case class Variable(name: String, p: Double, possible_values: List[Int]){}

class VariableSpec extends FlatSpec with Matchers {
  "A variable " should "return a list of possible values" in {
    val a = Variable("a", 0.1, List(0, 1))
    a.possible_values.head should be (0)
    a.possible_values.size should be (2)
  }
}

