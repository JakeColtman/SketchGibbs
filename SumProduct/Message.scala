package SumProduct
import org.scalatest._

trait Message{
  def variable: Variable
  def apply(value: Double): Double = value_at(variable<=value)
  def value_at(realization: Realization): Double
}


case class FactorMessage(factor: Factor) extends Message {

  if (factor.variables.size > 1) throw new Exception()

  def variable : Variable = factor.variables.head

  def value_at(realization: Realization): Double = {
    factor.rows.filter(x => x.realization == realization).head.value
  }
}

case class MockMessage() extends Message {
  override def variable: Variable = VariableFactory("mocked")
  override def value_at(realization: Realization): Double = 1.0
}

case object MessageFactory {
  def apply(factor: Factor) : Message = {
    FactorMessage(factor)
  }
  def apply = MockMessage()
}


class MessageSpec extends FlatSpec with Matchers {
  "A message " should " be able to extract its variable" in {
    val a = VariableFactory("a")
    val row = FactorRow(Realization(Map(a->0)), 1.0)
    val factor = RowsFactor(List(row))
    val message = FactorMessage(factor)
    message.variable should be (a)
  }

  "A message " should " fail if given a multi-variable parameter " in {
    val a = VariableFactory("a")
    val b = VariableFactory("b")
    val row = FactorRow(Realization(Map(a->0,b->0)), 1.0)
    an [Exception] should be thrownBy FactorMessage(RowsFactor(List(row)))
  }

  "A message " should " should be able to access the underlying factor " in {
    val a = VariableFactory("a")
    val row0 = FactorRow(Realization(Map(a->0)), 1.0)
    val row1 = FactorRow(Realization(Map(a->1)), 2.0)
    val factor = RowsFactor(List(row0,row1))
    val message = FactorMessage(factor)
    message.value_at(Realization(Map(a->0))) should be (1.0)
    message.value_at(Realization(Map(a->1))) should be (2.0)
  }

}
