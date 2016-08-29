package sampler.samplable

import sampler._
import sampler.empirical.Empirical
import sampler.maths.Random

trait Sampler{
  /** Collects samples from distribution until condition returns true
    *
    *  @param distribution Distribution object from which samples can be drawn
    *  @param protocol Characterisation of when convergence has taken place and sampling can stop
    *  @return Collection of sampled elements
    */
  def apply[T, D[_]: Samplable](distribution: D[T], r: Random)(protocol: ConvergenceProtocol[T]): Traversable[T]
}

/** Metric defining how the distance between two sets of data should be measured */
trait EmpiricalMetric {
  def distance[T](e1: Empirical[T], e2: Empirical[T]): Double
}

/** Implementation of [[sampler.samplable.EmpiricalMetric]] using the maximum distance statistical method */
trait MaxMetric extends EmpiricalMetric {
  def distance[T](e1: Empirical[T], e2: Empirical[T]): Double
  = e1.maxDistanceTo(e2)
}

trait MeanMetric extends EmpiricalMetric {
  def distance[T: Fractional](e1: Empirical[T], e2: Empirical[T]): Double
  = e1.meanDistanceTo(e2)
}

/** Determines whether a sequence of samples has converged. Requires mixin of a [[sampler.samplable.EmpiricalMetric]]
  *  implementation
  *
  *  @constructor Create a new convergence protocol given a chunk size and tolerance
  *  @param chunkSize the size of sampled chunks that have been used to build the sequence of samples
  *  @param tolerance the degree to which the samples can differ whilst being classed as converged
  *  @param maxRetries the number of samples to try be exiting without meeting convergence criteria
  */
abstract class ConvergenceProtocol[T](val chunkSize: Int, tolerance: Double, maxRetries: Int){
  this: EmpiricalMetric =>

  /** Tests a sequence of samples for convergence
    *
    *  @param seq Samples to be tested for convergence
    *  @return true/false according to if the samples had converged
    */
  def converged(seq: Seq[T]): Boolean = {
    val e1 = seq.take(seq.size - chunkSize).toEmpirical
    val e2 = seq.toEmpirical
    distance(e1, e2) < tolerance || seq.size > maxRetries
  }
}

/** Serializable implementation of [[sampler.samplable.Sampler]], uses until method of [[sampler.distribution.Distribution]]
  *  to sample from the distribution
  *
  *  {{{
  *  scala> import sampler.math.Random
  *  scala> import sampler.data._
  *
  *  scala> implicit val r: Random = Random
  *  scala> val dist = Distribution.uniform(0, 10)
  *  dist: sampler.data.Distribution[Int] = sampler.data.Distribution$$anon9@f4734e
  *
  *  scala> SerialSampler.apply(dist)(new ConvergenceProtocol[Int](10, 0.5, 100000) with MaxMetric)
  *  res0: Seq[Int] = Vector(7, 1, 9, 9, 9, 6, 0, 8, 6, 7)
  *  }}}
  */
object SerialSampler extends Sampler with Serializable{
  def apply[T, D[_]: Samplable](distribution: D[T], r: Random)(protocol: ConvergenceProtocol[T]) = {
    val chunkSize = protocol.chunkSize

    def takeMore(previous: Seq[T]): Seq[T] = {
      if(protocol.converged(previous)) previous
      else takeMore (
        previous ++ (distribution.until(_.length == chunkSize).sample(r))
      )
    }

    takeMore(distribution.until(_.length == chunkSize).sample(r))
  }
}

/** Parallelised implementation of [[sampler.samplable.Sampler]] allowing sampling of a distribution across
  *  multiple threads, which continuously takes batches of samples from the supplied distribution until
  *  a condition is met
  *
  *  {{{
  *  scala> import sampler.math.Random
  *  scala> import sampler.data._
  *
  *  scala> implicit val r: Random = Random
  *  scala> val dist = Distribution.uniform(0, 10)
  *  dist: sampler.data.Distribution[Int] = sampler.data.Distribution$$anon9@f4734e
  *
  *  scala> ParallelSampler.apply(dist)(new ConvergenceProtocol[Int](10, 0.5, 100000) with MaxMetric)
  *  res0: scala.collection.parallel.ParSeq[Int] = ParVector(3, 2, 7, 9, 1, 3, 7, 6, 7, 4)
  *  }}}
  *
  *  Warning: The supplied distribution must be thread safe to avoid propagation of error resulting from the
  *  concurrent calling of the sample method. E.g. In the Commons math normal distribution parallel sampling
  *  can sometimes give NaN due to concurrent access to a random number generator
  * {{{
  *  val normal = new NormalDistribution(0,0.1)
  *  (1 to 1000000000).par.foreach{i =>
  *      val r = normal.sample
  *      if(r.isNaN()) throw new Exception("r = "+r)
  *  }
  *  }}}
  *  Example taken from http://stackoverflow.com/questions/20969292/thread-safety-warnings
  *
  *  @constructor Create a new ParrallerSampler
  */
object ParallelSampler extends Sampler{
  def apply[T, D[_]: Samplable](distribution: D[T], r: Random)(protocol: ConvergenceProtocol[T]) = {
    val chunkSize = protocol.chunkSize

    def takeMore(previous: Seq[T]): Seq[T] = {
      if(protocol.converged(previous)) previous
      else takeMore(
        previous ++ (1 to chunkSize).par.map(i => distribution.sample(r))
      )
    }
    takeMore((1 to chunkSize).par.map(i => distribution.sample(r)).seq)
  }
}
