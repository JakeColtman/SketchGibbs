package SumProduct
import org.scalatest._

trait IMessage{
  def variable: Variable
  def value_at(realization: Realization): Double
}

case class Message(factor: Factor) extends IMessage {

  def variable : Variable = factor.variables.head

  def value_at(realization: Realization): Double = {
    factor.rows.filter(x => x.realization == realization).head.value
  }
}


class MessageSpec extends FlatSpec with Matchers {
  "A message " should " be able to extract its variable" in {
    true
  }

  "A message " should " fail if given a multi-variable parameter " in {
    true
    //n [IndexOutOfBoundsException] should be thrownBy s.charAt(-1)
  }

  "A message " should " return a value for all realizations of a variable " in {
    true
  }

}
