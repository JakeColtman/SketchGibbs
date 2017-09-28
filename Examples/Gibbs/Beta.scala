package Examples.Gibbs

/**
  * Created by jacoltman on 26/09/2017.
  */
object Beta extends App {
  import org.apache.commons.math3.distribution.BetaDistribution

  val a = 150
  val b = 150
  println(new BetaDistribution(a, b).density(0.5))

}
