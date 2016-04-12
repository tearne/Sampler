package farrington_old

import scala.annotation.tailrec
import org.apache.commons.math3.distribution.PoissonDistribution
import scala.util.Random
import breeze.stats.distributions.Poisson
import breeze.stats.distributions.NegativeBinomial
import breeze.stats.distributions.LogNormal
import breeze.stats.distributions.Binomial
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.Charset

/*
  =========
  NOTES:
  Function to simulate outbreak data for the Early Detection System.
    
  Uses a negative binomial model if dispersion parameter > 1.
  Uses a Poission distribution if dispersion parameter == 1.
  
  Follows the method outlined in
  Noufaily et al., Statist. Med. 2013 (32) 1206-1222
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon
  Date:      27/02/2015
  Last edit: 10/03/2015
  
  ==========
  INPUTS:

  nData           No. of time intervals for which to simulate data
  endYear         Last year in which outbreak data should be simulated
  outbreakShape   Choose "logNormal" or "epidemicCurve" shape
  outbreakLength  Choose "long" or "short" outbreak (~7 or ~3 months)
  
  
  alpha           Frequency of reports
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
  
  genPoisson            Generate a nonzero number from a Poisson distribution (mean = 0, sd = 0.5)
  stdDev                Calculates standard deviation
  atanh                 Inverse hyperbolic tan function  
  sumFunction           Performs sum of a function of an integer from a to b
  sumTerm               Function of j which is to be summed from j= 1 to m:
  addList               Adds a list of values at the specified indices to a given sequence
  runBaseline           Simulates baseline data using a Negative Binomial distribution
  logNormalOutbreak     Simulates outbreak data using a log-Normal distribution
  epidemicCurveOutbeak  Simulates outbreak data using an epidemic curve shape
  addOutbreak           Simulates an outbreak and adds to baseline data
  splitOutbreak         Splits an outbreak into two counts using a binomial distribution
  
  =========  
  OUTPUT:
  
  counts          Outbreak counts at each month
  hist            Binned count data corresponding to outbreak
  start           Starting month of outbreak
    
  */

case class GenerationParams(
    alpha: Double,
    beta: Double,
    m: Int,
    gamma_1: Double,
    gamma_2: Double,
    dispersion: Double
  )
object GenerationParams{
  val default = GenerationParams(1.5, 0, 1, 0.2, -0.4, 1)
}

case class GenerationResult(
    year: IndexedSeq[Int],
    month: IndexedSeq[Int],
    baseline: IndexedSeq[Int],
    counts: IndexedSeq[Int],
    hist: List[(Int, Int)],
    start: Int,
    end: Int,
    min: Int,
    max: Int
  )
  
case class BaselineResult(
    year: IndexedSeq[Int],
    month: IndexedSeq[Int],
    baseline: IndexedSeq[Int],
    mean: IndexedSeq[Double]
  )
  
object GenerateData {
  
  def run(
      nData: Int,
      endYear: Int,
      outbreakShape: String,
      outbreakLength: String,
      endPreOutbreak: Int,
      endOutbreak: Int,
      magnitude: Double,
      params: GenerationParams = GenerationParams.default
    ): GenerationResult = {
    
    import params._
    
    // Construct sequences of months and years
    val startYear = math.round(endYear - nData.toDouble/12)  
    val year = (1 to nData).map(i => (startYear + ((i-1) / 12)).toInt)
    val month = (1 to nData).map(i => (i-1) % 12 + 1)  
    
    //=======================
    //Simulate baseline data
    
    // Function of j which is to be summed from j= 1 to m:
    def sumTerm(t: Int, j: Int): Double = {
      gamma_1*math.cos((2*math.Pi*j*t).toDouble / 12) + 
      gamma_2*math.sin((2*math.Pi*j*t).toDouble / 12)
    }
    
    // Calculate the mean of the baseline data
    val mean = (1 to nData).map(i =>
      math.exp(alpha + beta*i + sumFunction(sumTerm)(i,1,m)) )
    
    val lower = mean.map(i => i - 1.96*math.sqrt(i))
    val upper = mean.map(i => i + 1.96*math.sqrt(i))
    
//    println("Mean cases for baseline data = ")
//    println(mean)
//    println("95% confidence interval (lower) = ")
//    println(lower)
//    println("95% confidence interval (upper) = ")
//    println(upper)
    
    // Sample baseline data from a Poisson or Negative Binomial distribution
    val baseline = 
    if (dispersion == 1) {
      val poi = mean.map(i => new Poisson(i))
      poi.map(i => i.draw())
    }
    else {
      val n = mean.map(i => i / (dispersion - 1))
      val p = 1 - math.pow(dispersion,-1)
      val nb = n.map(i => new NegativeBinomial(i,p))
      nb.map(i => i.draw())
    }
    
    //=======================
    //Simulate outbreak data
    
    val rnd = new Random  
    val tOutbreak = (endPreOutbreak + 1) + rnd.nextInt(endOutbreak - endPreOutbreak)
    
//    // Calculate standard deviation of the baseline count at tOutbreak
//    val std = stdDev(mean, dispersion)
//    
//    val sd = std(tOutbreak - 1)
//    //val sd = math.abs(baseline(tOutbreak-1)-mean(tOutbreak-1))
//    
//    // Calculate number of outbreak cases from Poisson distribution
//    val poi_outbreak = new Poisson(magnitude * sd)    
//    val n = genPoisson(poi_outbreak)
//    //println("No. of outbreak cases = " + n)
    
    val nBaseline = (0 until 7).map(i => baseline(tOutbreak-1+i)).sum
    val n = math.round(magnitude * nBaseline).toInt
    
    // Calculate outbreak and distribute
    val outbreakDistribution =
      if (outbreakShape == "logNormal") logNormalOutbreak(n, outbreakLength)
      else epidemicCurveOutbreak(n, outbreakLength)
    //println(outbreakDistribution)
    
    // Count number of outbreak cases for each month of the outbreak
    val outbreakHist = 
      outbreakDistribution.groupBy(w => w).mapValues(_.size).toList.sorted
    //println(outbreakHist)
    
    val outbreakMonths = outbreakHist.map(i => i._1)
    val min =
      if ((outbreakMonths.min to outbreakMonths.max).diff(outbreakMonths).isEmpty) {
        outbreakHist.map(i => i._2).min
      }
      else 0
    val max = outbreakHist.map(i => i._2).max
      
    // Create list of pairs of time index and number of cases to be added
    val outbreakIdx =
      outbreakHist.map{case (key, value) => (key+tOutbreak-1, value)}
    
    // Last month of outbreak
    val tEnd = outbreakIdx.last._1 + 1
    
    // Add to baseline data to return simulated outbreak data
    val dataOutbreak = addList(baseline,outbreakIdx)
    
    GenerationResult(year, month, baseline, dataOutbreak, outbreakHist, tOutbreak, tEnd, min, max)
        
  }
  
  @tailrec
  def genPoisson(rng: Poisson): Int = {
    val nTry = rng.sample()
    if (nTry != 0) nTry
    else genPoisson(rng: Poisson)
  }
  
  // Standard deviation
  def stdDev(
      mean: IndexedSeq[Double],
      dispersion: Double
    ): IndexedSeq[Double] = {
    
    val dataBaseline = (0 until 10).map( j =>
      if (dispersion == 1) {
        val poi = mean.map(i => new Poisson(i))
        poi.map(i => i.draw())
      }
      else {
        val n = mean.map(i => i / (dispersion - 1))
        val p = 1 - math.pow(dispersion,-1)
        val nb = n.map(i => new NegativeBinomial(i,p))
        nb.map(i => i.draw())
      }
    )
    val meanBL = dataBaseline.map(i => i.sum.toDouble / i.length)

    val n = dataBaseline(0).length

    val sumOfSquares = (0 until n).map(i =>
      (0 until 10).map(j =>
        math.pow(dataBaseline(j)(i) - meanBL(j), 2)).sum)
    (0 until n).map(i => math.sqrt(sumOfSquares(i))/10)
    
  }
  
  // Inverse hyperbolic tan
  def atanh(x: Double) = 0.5 * ( math.log(1.0 + x) - math.log(1.0 - x) )
  
  // Performs sum of a function of an integer from integer a to integer b
  def sumFunction(f: (Int, Int) => Double)(x: Int, a: Int, b: Int): Double = {
    @tailrec
    def loop(a: Int, acc: Double): Double = {
      if (a > b) acc
      else loop(a+1, acc + f(x, a))
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
  
  
  def runBaseline(
      nData: Int,
      endYear: Int,
      params: GenerationParams = GenerationParams.default
    ): BaselineResult = {
    
    import params._
    
    // Construct sequences of months and years
    val startYear = math.round(endYear - nData.toDouble/12)  
    val year = (1 to nData).map(i => (startYear + ((i-1) / 12)).toInt)
    val month = (1 to nData).map(i => (i-1) % 12 + 1)  
    
    //=======================
    //Simulate baseline data
    
    // Function of j which is to be summed from j= 1 to m:
    def sumTerm(t: Int, j: Int): Double = {
      gamma_1*math.cos((2*math.Pi*j*t).toDouble / 12) + 
      gamma_2*math.sin((2*math.Pi*j*t).toDouble / 12)
    }
    
    // Calculate the mean of the baseline data
    val mean = (1 to nData).map(i =>
      math.exp(alpha + beta*i + sumFunction(sumTerm)(i,1,m)) )
      
    // Sample baseline data from a Poisson or Negative Binomial distribution
    val baseline = 
    if (dispersion == 1) {
      val poi = mean.map(i => new Poisson(i))
      poi.map(i => i.draw())
    }
    else {
      val n = mean.map(i => i / (dispersion - 1))
      val p = 1 - math.pow(dispersion,-1)
      val nb = n.map(i => new NegativeBinomial(i,p))
      nb.map(i => i.draw())
    }
    
    BaselineResult(year, month, baseline, mean)
    
  }
  
  
  def logNormalOutbreak(n: Int, outbreakLength: String) = {    
    if (outbreakLength == "short")
      LogNormal(0,0.5).sample(n).sorted.map(x => math.floor(x).toInt)
    else
      LogNormal(0,0.5).sample(n).sorted.map(x => math.floor(2*x).toInt)
  }
  
  
  def epidemicCurveOutbreak(n: Int, outbreakLength: String) = {
    val rnd = new Random
    val Finv = (1 to n).map(x => atanh(2*rnd.nextDouble() - 1))
    val maxF = Finv.max
    val Finv_round = 
      if (outbreakLength == "short")
        Finv.map(x => math.ceil(x).toInt)
      else
        Finv.map(x => math.ceil(2*x).toInt)
    val count = 
      if (Finv_round.min < 0)
        Finv_round.map(x => (x - Finv_round.min) + 1)
      else
        Finv_round
    count
  }
  
  def addOutbreak(
      dataBaseline: BaselineResult,
      outbreakShape: String,
      outbreakLength: String,
      endPreOutbreak: Int,
      endOutbreak: Int,
      magnitude: Double,
      params: GenerationParams = GenerationParams.default
    ): GenerationResult = {
    
    import dataBaseline._
    
    val nData = baseline.length

    val rnd = new Random  
    val tOutbreak = (endPreOutbreak + 1) + rnd.nextInt(endOutbreak - endPreOutbreak)
    
//    // Calculate standard deviation of the baseline count at each tOutbreak
//    val std = stdDev(mean, params.dispersion)    
//    val sd = std(tOutbreak - 1)
//    
//    // Calculate number of outbreak cases from Poisson distribution
//    val poi_outbreak = new Poisson(magnitude * sd)
//    val n = genPoisson(poi_outbreak)
    
    val nBaseline = (0 until 7).map(i => baseline(tOutbreak-1+i)).sum
    val n = math.round(magnitude * nBaseline).toInt
    
    // Calculate outbreak and distribute
    val outbreakDistribution =
      if (outbreakShape == "logNormal") logNormalOutbreak(n, outbreakLength)
      else epidemicCurveOutbreak(n, outbreakLength)
      
    val min = outbreakDistribution.min
    val max = outbreakDistribution.max
    
    // Count number of outbreak cases for each month of the outbreak
    val outbreakHist = 
      outbreakDistribution.groupBy(w => w).mapValues(_.size).toList.sorted
      
    // Create list of pairs of time index and number of cases to be added
    val outbreakIdx =
      outbreakHist.map{case (key, value) => (key+tOutbreak-1, value)}
    
    // Last month of outbreak
    val tEnd = outbreakIdx.last._1 + 1
    
    // Add to baseline data to return simulated outbreak data
    val dataOutbreak = addList(baseline,outbreakIdx)
    
    GenerationResult(year, month, baseline, dataOutbreak, outbreakHist, tOutbreak, tEnd, min, max)     
    
  }
  
  
  def splitOutbreak(hist: List[(Int, Int)], start: Int) = {
    
    // Split outbreaks according to a binomial model
    val count1 = hist.map{ 
      case (key, value) => (key, Binomial(value, 0.5).draw())
    }
    val count1_indexed = count1.zipWithIndex
    val count2_indexed = count1_indexed.map{
      case ((key, value), i) => ((key, hist(i)._2 - value), i)
    }
    val (count2, index) = count2_indexed.unzip
  
    val outbreak1 =
      count1.map{case (key, value) => (key+start-1, value)}    
    val outbreak2 =
      count2.map{case (key, value) => (key+start-1, value)}
    
    //println("full = " + hist)
    //println("count1 = " + count1)
    //println("count2 = " + count2)
    (outbreak1, outbreak2)
    
  }
      

}
