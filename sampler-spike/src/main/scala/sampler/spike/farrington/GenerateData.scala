package sampler.spike.farrington

import scala.annotation.tailrec
import org.apache.commons.math3.distribution.PoissonDistribution
import scala.util.Random
import breeze.stats.distributions.Poisson
import breeze.stats.distributions.NegativeBinomial
import breeze.stats.distributions.LogNormal

/*
  =========
  NOTES:
  Function to simulate outbreak data for the EDS.
    
  Uses a negative binomial model if dispersion parameter > 1
  Uses a Poission distribution if dispersion parameter == 1
  
  Follows the method outlined in
  Noufaily et al., Statist. Med. 2013 (32) 1206-1222
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon
  Date:      27/02/2015
  Last edit: 27/02/2015
  
  ==========
  INPUTS:

  nData          No. of time intervals for which to simulate data
  
  alpha           
  beta            Linear trend
  m               Seasonality (0 = none, 1 = annual, 2 = biannual)
  gamma_1         Controls magnitude of peaks
  gamma_2         Controls magnitude of troughs
  dispersion      Dispersion parameter
  
  k               Parameter which controls the magnitude of the outbreak
  
  outbreakLength  Length of outbreak ("short" or "long")
  endPreOutbreak  End of pre-outbreak period 
  endOutbreak     End of outbreak period
  
  =========
  FUNCTIONS:
  
  stdDev          Calculates standard deviation
  sumFunction     Performs sum of a function of an integer from a to b
  sumTerm         Function of j which is to be summed from j= 1 to m:
  addList         Adds a list of values at the specified indices to a given sequence
  
  =========  
  OUTPUT:
  
  Simulated data with outbreak
    
  */

case class GenerationParams(
      alpha: Double,
      beta: Double,
      m: Int,
      gamma_1: Double,
      gamma_2: Double,
      dispersion: Double,
      k: Double
    )
object GenerationParams{
  val default = GenerationParams(1.5, 0, 1, 0.2, -0.4, 1, 10)
}

object GenerateData {
  
  def run(
      nData: Int,
      outbreakLength: String,
      endPreOutbreak: Int,
      endOutbreak: Int,
      params: GenerationParams = GenerationParams.default
    ): IndexedSeq[Int] = {
    
    import params._
    
    //=======================
    //Simulate baseline data
    
    // Function of j which is to be summed from j= 1 to m:
    def sumTerm(j: Int): Double = {
      gamma_1*math.cos((2*math.Pi*j*nData).toDouble / 12) + 
      gamma_2*math.sin((2*math.Pi*j*nData).toDouble / 12)
    }
    
    // Calculate the mean of the baseline data
		val mean_baseline = math.exp(alpha + beta*nData + sumFunction(sumTerm)(1,m))
    
    // Sample baseline data from a Poisson or Negative Binomial distribution
    val dataBaseline = 
    if (dispersion == 1) {
      val poi = new Poisson(mean_baseline)
      poi.sample(nData)
    }
    else {
      val n = mean_baseline / (dispersion - 1) // number of failures
      val p = 1 - math.pow(dispersion,-1)      // probability of success
      val nb = new NegativeBinomial(n,p)
      nb.sample(nData)
    }
    
    //=======================
    //Simulate outbreak data
    
    val rnd = new Random  
    val tOutbreak = (endPreOutbreak + 1) + rnd.nextInt(endOutbreak - endPreOutbreak)
    
    // Calculate standard deviation of the baseline count at each tOutbreak
    def stdDev(data: IndexedSeq[Int], mean: Double): Double = {
      data.map(x => math.pow(x - mean, 2)).sum / nData
    }
    val sd = stdDev(dataBaseline, mean_baseline)
    
    // Calculate no. of outbreak cases to simulate
    val poi_outbreak = new Poisson(k * sd)
    val nCases = poi_outbreak.sample()
    
    // Distribute over required period (~3 months for short, ~6 months for long)
    val outbreakDistribution = 
      if (outbreakLength == "short")
        LogNormal(0,0.5).sample(nCases).sorted.map(x => math.floor(x).toInt)
      else
        LogNormal(0,0.5).sample(nCases).sorted.map(x => math.floor(2*x).toInt)
    
    // Count number of outbreak cases for each month of the outbreak
    val outbreakHist = 
      outbreakDistribution.groupBy(w => w).mapValues(_.size).toList.sorted
    
    // Create list of pairs of time index and number of cases to be added
    val outbreakIdx =
      outbreakHist.map{case (key, value) => (key+tOutbreak-1, value)}
    
    // Add to baseline data to return simulated outbreak data
    addList(dataBaseline,outbreakIdx)
        
  }
  
  //=======================
  // Function definitions
  
  // Performs sum of a function of an integer from integer a to integer b
  def sumFunction(f: Int => Double)(a: Int, b: Int): Double = {
    @tailrec
    def loop(a: Int, acc: Double): Double = {
      if (a > b) acc
      else loop(a+1, acc + f(a))
    }
    loop(a,0)    
  }
        
  // Adds a list of values at the specified indices to a given sequence
  def addList(
      current: IndexedSeq[Int],
      toDo: List[(Int, Int)]
    ): IndexedSeq[Int] = {
      @tailrec
      def loop(update: IndexedSeq[Int], toDo: List[(Int, Int)]): IndexedSeq[Int] = {
        if (toDo.size == 0) update
        else loop(update.updated(toDo.head._1, update(toDo.head._1) + toDo.head._2), toDo.tail)
  }
    loop(current,toDo)
  }

}
