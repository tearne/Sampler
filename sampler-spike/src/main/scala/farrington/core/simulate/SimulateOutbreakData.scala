package farrington.core.simulate

import scala.annotation.tailrec
import scala.util.Random
import breeze.stats.distributions.NegativeBinomial
import breeze.stats.distributions.Poisson
import breeze.stats.distributions.LogNormal
import breeze.stats.distributions.Binomial

object SimulateOutbreakData {
  
  // HELPER FUNCTIONS:
  
  // Performs sum of a function of an integer from integer a to integer b
  def sumFunction(f: (Int, Int) => Double)(x: Int, a: Int, b: Int): Double = {
    @tailrec
    def loop(a: Int, acc: Double): Double = {
      if (a > b) acc
      else loop(a+1, acc + f(x, a))
    }
    loop(a ,0)    
  }
  
  // Adds a list of values at the specified indices to a given sequence
  def addList(current: IndexedSeq[Int], toDo: List[(Int, Int)]): IndexedSeq[Int] = {
    @tailrec
    def loop(update: IndexedSeq[Int], toDo: List[(Int, Int)]): IndexedSeq[Int] = {
      if (toDo.size == 0) update
      else loop(update.updated(toDo.head._1, update(toDo.head._1) + toDo.head._2), toDo.tail)
    }
    loop(current, toDo)
  }
  
  // Inverse hyperbolic tan
  def atanh(x: Double) = 0.5 * ( math.log(1.0 + x) - math.log(1.0 - x) )
  
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
  
  def runBaseline(
      nData: Int,
      endYear: Int,
      params: SimulationParams = SimulationParams.default
    ): BaselineData = {
    
    import params._
    
    // Construct sequences of months and years
    val startYear = math.round(endYear - nData.toDouble/12)  
    val year = (1 to nData).map(i => (startYear + ((i-1) / 12)).toInt)
    val month = (1 to nData).map(i => (i-1) % 12 + 1)  
    
    // Function of j which is to be summed from j= 1 to m:
    def sumTerm(t: Int, j: Int): Double = {
      gamma_1 * math.cos((2*math.Pi*j*t).toDouble / 12) + 
      gamma_2 * math.sin((2*math.Pi*j*t).toDouble / 12)
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
    
    BaselineData(year, month, baseline, mean)
    
  }
  
  def addOutbreak(
      baselineData: BaselineData,
      outbreakShape: String,
      outbreakLength: String,
      endPreOutbreak: Int,
      endOutbreak: Int,
      magnitude: Double,
      params: SimulationParams = SimulationParams.default
    ): OutbreakData = {
    
    import baselineData._
    
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
    
    OutbreakData(year, month, baseline, dataOutbreak, outbreakHist, tOutbreak, tEnd, min, max)     
    
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
  
  // SIMULATION:
  
  def run(
      nData: Int,
      endYear: Int,
      outbreakShape: String,
      outbreakLength: String,
      endPreOutbreak: Int,
      endOutbreak: Int,
      magnitude: Double,
      params: SimulationParams = SimulationParams.default
    ): OutbreakData = {
    
    val baselineData = runBaseline(nData, endYear, params)
    
    addOutbreak(baselineData, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude, params)
    
  }

}