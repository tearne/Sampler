package sampler.spike

import sampler.math.Random
import sampler.math.StatisticsComponent
import sampler.data.Empirical
import sampler.data.Samplable

class EmpiricalProduct[A](val emps: IndexedSeq[Empirical[A]]) extends StatisticsComponent{

  private def product(acc: Int, remaining: IndexedSeq[Int]): Int = {
    if(remaining.length == 0) acc
    else product(acc * remaining.head, remaining.tail)
  }
  
  lazy val size = product(1, emps.map(_.size))
  
  def toSamplable(implicit r: Random) = new Samplable[IndexedSeq[A]]{
    val samps = emps.map(_.toSamplable)
    
    def sample = samps.map(_.sample)
  }
  
  def expectation(implicit num: Fractional[A]) = emps.map(mean(_))
  
  def distTo(that: EmpiricalProduct[A])(implicit num: Fractional[A]) = {
    // TODO implement
    
    // What are we trying to find the distance between, two lots of products, or Empiricals within the products
    // Suggested implementation was a.zip(b).map(a.dist(b))
    // How would this work if a and b were of different lengths?
  }
}