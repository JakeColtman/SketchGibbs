package SumProduct
import org.scalatest.{FlatSpec, Matchers}
import breeze.stats.distributions.Gaussian

import scala.util.Random

/**
  * Created by jacoltman on 24/09/2017.
  */
trait Sampler {
  def draw : Double
}

case class LogSliceSampler(distribution: Distribution, previous_value: Double) extends Sampler {

  val width = 0.4
  val current_prob: Double = distribution.value_at(previous_value)

  private def uniform_draw_in_range(lower_bound: Double, upper_bound: Double) : Double = {
    val rand_double = new Random().nextDouble()
    lower_bound + (upper_bound - lower_bound) * rand_double
  }

  private def slice_boundary(threshold: Double, starting_value: Double, direction: Double, w: Double) : Double = {
    val new_loc = starting_value + (direction * w)
    if (distribution.value_at(new_loc) < threshold) new_loc
    else {
      slice_boundary(threshold, new_loc, direction, w * 2)
    }
  }

  private def choose_height(): Double = {
    uniform_draw_in_range(0.0, current_prob)
  }

  def draw : Double = {
    val height = choose_height()
    var left_boundary = slice_boundary(height, previous_value, -1.0, width)
    var right_boundary = slice_boundary(height, previous_value, 1.0, width)
    var found = false
    var proposal = previous_value
    while (!found){
      proposal = uniform_draw_in_range(left_boundary, right_boundary)
      found = distribution.value_at(proposal) > height
      if (!found & proposal < previous_value){
        left_boundary = proposal
      }
      if (!found & proposal > previous_value){
        right_boundary = proposal
      }
    }
    proposal
  }

  def sample(n: Int, current_result: List[Double] = List()) : List[Double] = {
    if (n == 0) current_result
    else{
      val prev_value = current_result.head
      val next_sample = SliceSampler(distribution, prev_value).draw
      sample(n - 1, List(next_sample) ++ current_result)
    }
  }
}

case class SliceSampler(distribution: Distribution, previous_value: Double) extends Sampler {

  val width = 0.4
  val current_prob: Double = distribution.value_at(previous_value)

  private def uniform_draw_in_range(lower_bound: Double, upper_bound: Double) : Double = {
    val rand_double = new Random().nextDouble()
    lower_bound + (upper_bound - lower_bound) * rand_double
  }

  private def slice_boundary(threshold: Double, starting_value: Double, direction: Double, w: Double) : Double = {
    val new_loc = starting_value + (direction * w)
    if (distribution.value_at(new_loc) < threshold) new_loc
    else {
      slice_boundary(threshold, new_loc, direction, w * 2)
    }
  }

  private def choose_height(): Double = {
    uniform_draw_in_range(0.0, current_prob)
  }

  def draw : Double = {
    val height = choose_height()
    var left_boundary = slice_boundary(height, previous_value, -1.0, width)
    var right_boundary = slice_boundary(height, previous_value, 1.0, width)
    var found = false
    var proposal = previous_value
    while (!found){
      proposal = uniform_draw_in_range(left_boundary, right_boundary)
      found = distribution.value_at(proposal) > height
      if (!found & proposal < previous_value){
        left_boundary = proposal
      }
      if (!found & proposal > previous_value){
        right_boundary = proposal
      }
    }
    proposal
  }

  def sample(n: Int, current_result: List[Double] = List()) : List[Double] = {
    if (n == 0) current_result
    else{
      val prev_value = current_result.head
      val next_sample = SliceSampler(distribution, prev_value).draw
      sample(n - 1, List(next_sample) ++ current_result)
    }
  }
}

class SliceSamplerSpec extends FlatSpec with Matchers {
  "A slice sampler " should " closely approximate the true mean" in {
    val variable = VariableFactory("a")
    val true_mean = 7.0
    val true_stdev = 0.5

    def f(realization: Realization): Double = Gaussian(true_mean, true_stdev).pdf(realization.realization.values.head)
    val ff : (Realization => Double) = f
    val distr = DistributionFactory(variable, ff)
    val sampler = SliceSampler(distr, 1.0)
    val samples = sampler.sample(1000, List(true_mean + 5))
    val mean = samples.sum / samples.size
    mean should be (true_mean +- 0.1)
    val variance = samples.map(x => (x - mean) * (x -mean)).sum / samples.size
    variance should be (true_stdev * true_stdev +- 0.1)
    println(variance)
  }
}