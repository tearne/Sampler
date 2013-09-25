package sampler.spike

import sampler.math.Random
import sampler.math.StatisticsComponent
import sampler.data.Empirical
import sampler.data.Samplable
import sampler.math.StatisticsComponent

class EmpiricalProduct[A](val emps: IndexedSeq[Empirical[A]]) extends StatisticsComponent {

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
    // TODO improve this draft implementation
    
    assert(emps.size == that.emps.size)
    
    val distances = emps.zip(that.emps).map(pair => meanDistance(pair._1, pair._2))
   
    distances.max
       
    // This is the first thought potential implementation
    // May not be necessary, or the best method
  }
}