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

}
