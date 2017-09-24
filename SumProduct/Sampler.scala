package SumProduct

/**
  * Created by jacoltman on 24/09/2017.
  */
trait Sampler {
  def sample(distribution: Distribution) : Double
}

case object SliceSampler {
  def sample(distribution: Distribution) = {
    0.0
  }
}