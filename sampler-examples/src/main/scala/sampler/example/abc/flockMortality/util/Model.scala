package sampler.example.abc.flockMortality.util

import scala.language.existentials
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import sampler.abc.Prior
import sampler.distribution.Distribution
import sampler.maths.Random
import sampler._

case class Model(observed: IndexedSeq[Observed], interval: IntervalPrior) extends sampler.abc.Model[Parameters] {

  val numSheds = observed.size
  
  val meanEggs = observed.map(obs => obs.meanEggs)
  val eggCoeff = observed.map(obs => obs.eggCoeff)
  
   // Include baseline mortality
//  val baselineMortalityRate = observed.map{ obs => 0.0 }  // Use this to turn off baseline mortality
  val baselineMortalityRate = observed.map{ obs =>
    val baselineDeaths = obs.dead.take(obs.infectionFreeDays)
    val meanDeaths = (baselineDeaths.sum.toDouble / baselineDeaths.length)
    meanDeaths / obs.flockSize
  }  
  val baselineDeaths = (0 until observed.length).map{ i => baselineMortalityRate(i) * observed(i).flockSize }
  
  // Prior probability density and draw functions
  val prior = new Prior[Parameters]{
    def density(p: Parameters): Double = IntervalPrior.density(p, interval)    
    val distribution = Distribution.from(
      r => IntervalPrior.draw(numSheds, interval)(r)
    )
  }
  
  //===
  // Kernel
  case class Kernel(lower: Double, upper: Double) {
    
    val width = upper - lower
    
    val kernel = new Prior[Double]{
    
      val normal = {
        val syncRand: RandomGenerator = new SynchronizedRandomGenerator(new MersenneTwister())
        new NormalDistribution(syncRand, 0, width / 20, NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
      }
          
      def density(at: Double) = {
        normal.density(at)
      }

      val distribution = Distribution.from{random =>
        val r = normal.sample
        if(r.isNaN() || r.isInfinite()) {
          val e = new Exception("here... r = " + r)
          e.printStackTrace()
          throw e
        }
        r
      }
      
    }
  
  }
  
  val betaRange = Kernel(interval.beta.head, interval.beta.last)
  val etaRange = Kernel(interval.eta.head, interval.eta.last)
  val gammaRange = Kernel(interval.gamma.head, interval.gamma.last)
  val deltaRange = Kernel(interval.delta.head, interval.delta.last)
  val sigmaRange = Kernel(interval.sigma.head, interval.sigma.last)
  val sigma2Range = Kernel(interval.sigma2.head, interval.sigma2.last)
  val offsetRange = Kernel(interval.offset.head, interval.offset.last)
  
  def perturb(p: Parameters) = {
    implicit val random = Random
    // TODO can't stay the same?
    val threeDie = Distribution.uniform(-1, 1)
    /*
      cf:
    	val threeDie = Distribution.uniform(IndexedSeq(-1,0,1))
	    private def threeDensity(v: Int) = if(v <= 1 || v >= -1) 1.0 / 3 else 0.0
     */
    import p._
    Parameters(
      beta + betaRange.kernel.distributionSupportChecked.sample,
      eta + etaRange.kernel.distributionSupportChecked.sample,
      gamma + gammaRange.kernel.distributionSupportChecked.sample,
      delta + deltaRange.kernel.distributionSupportChecked.sample,
      sigma + sigmaRange.kernel.distributionSupportChecked.sample,
      sigma2 + sigma2Range.kernel.distributionSupportChecked.sample,
      offset.map(i => i + threeDie.sample)
    )
  }
  
  def perturbDensity(a: Parameters, b: Parameters) = {
    def threeDensity(v: Int) = if(v <= 1 || v >= -1) 1.0 / 3 else 0.0
    val offsetProduct =
      (0 until a.offset.length).map(i => threeDensity(a.offset(i) - b.offset(i))).product
    betaRange.kernel.density(a.beta - b.beta) *
    etaRange.kernel.density(a.eta - b.eta) *
    gammaRange.kernel.density(a.gamma - b.gamma) *
    deltaRange.kernel.density(a.delta - b.delta) *
    sigmaRange.kernel.density(a.sigma - b.sigma) *
    sigma2Range.kernel.density(a.sigma2 - b.sigma2) *
    offsetProduct
  }
  
  //===
  // Model  
  def distanceToObservations(p: Parameters): Distribution[Double] = modelDistribution(p).map(error => error.distanceToObserved)
  
  def modelDistribution(p: Parameters) = {
    
    val sheds = 0 until observed.length    
    val numDays = observed.map(i => i.days.length)
    
    // Include extra days if offset < 0
    val odeDays =
      if (p.offset.filter(i => i < 0).length > 0) numDays.map(_ - p.offset.min)
      else numDays
    
    def solve(): SimulatedResult = {
      
      val maps = sheds.map{ i =>
      
        val numDeaths = baselineDeaths(i) * (p.offset(i) + 1)
        val numBirds = observed(i).flockSize - numDeaths - 1
        val y0 = Array(numBirds.toDouble, 1.0, 0.0, 0.0, numDeaths)
        
        val integrated = ODEState.rounded(ODE.run(y0, odeDays(i), p, eggCoeff(i), baselineMortalityRate(i)))
        
        // Create map of ODE solution without offset
        val unshiftedResultsMap =
          (0 until odeDays(i))
            .foldLeft(Map[Int, ODEState]()){ case (map, day) =>
              map.updated(day, integrated(day))
            }
        
        // Make appropriate adjustment for offset (shift forwards or backwards)
        val shiftedResultsMap =
          if (p.offset(i) == 0) unshiftedResultsMap
          else if (p.offset(i) > 0) {
            val tmp = unshiftedResultsMap.map{case (key, value) =>
                (key + p.offset(i)) -> value
            }
            val additions = 
              (0 until p.offset(i)).foldLeft(Map[Int, ODEState]()){ case (map, day) =>
                val numDeaths = (day + 1) * baselineDeaths(i)
                val numBirds = observed(i).flockSize - numDeaths
                map.updated(day, ODEState(numBirds, 0, 0, 0, numDeaths, unshiftedResultsMap(0).eggs))
              }
            tmp.++(additions)
          }
          else if (p.offset(i) < 0) {
            val tmp = unshiftedResultsMap.map{case (key, value) =>
                (key + p.offset(i)) -> value
            }
            tmp.filter{case (key, value) => key >= 0}
          }
          else throw new RuntimeException("not supported yet")
        
          val finalMap =
            shiftedResultsMap
              .filter{ case (key, value) => key < numDays(i) }
              
          finalMap
                
      }
      
      SimulatedResult(maps.toIndexedSeq, observed, p)
      
    }
        
    // Deterministic model will always return the same answer
    Distribution.always(solve)

  }
  
}