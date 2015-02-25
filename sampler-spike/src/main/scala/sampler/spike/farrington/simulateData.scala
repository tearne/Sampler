package sampler.spike.farrington

import org.apache.commons.math3.distribution.PoissonDistribution
import scala.util.Random
import breeze.stats.distributions.LogNormal
import scala.annotation.tailrec
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.Charset
import sampler.r.process.ScriptRunner


/*
  =========
  NOTES:
  Function simulates outbreak data for the EDS using a negative binomial
  model with dispersion parameter >= 1
  (Note that a Poission distribution is used if dispersion parameter == 1)
  
  Follows the method outlined in
  Noufaily et al., Statist. Med. 2013 (32) 1206-1222
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon
  Date:      19/02/2015
  Last edit: 24/02/2015
  
  ==========
  INPUTS:

	nYears       No. of years for which to simulate data
	period       Intervals at which to simulate data ("weekly" or "monthly")
	
	alpha        
	beta         Linear trend
	m            Seasonality (0 = none, 1 = annual, 2 = biannual)
	gamma_1      Controls magnitude of peaks
	gamma_2      Controls magnitude of troughs
	dispersion   Dispersion parameter
	
	=========
	OUTPUTS:
		
	sim_data   Simulated outbreak data
 
 */

object simulateData  extends App {
  

  //=======================
  // User-defined parameters
  
  // Number of years for which to simulate data:
  val nYears = 38.5
  
  // Choose to simulate data each "week" or "month"
  // period = "week"
  val period = "month"
  
  // Choose "short" or "long" outbreaks
  // outbreakLength = "short"
  val outbreakLength = "long"
  
  // Set of parameters to control baseline data
  val alpha = 1.5
  val beta = 0
  val m = 1
  val gamma_1 = 0.2
  val gamma_2 = -0.4
  val dispersion = 1.0
  
  // Set of parameters to control outbreak data
  val k = 20
  
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
  def addList(current: IndexedSeq[Int], toDo: List[(Int, Int)]): IndexedSeq[Int] = {
    @tailrec
    def loop(update: IndexedSeq[Int], toDo: List[(Int, Int)]): IndexedSeq[Int] = {
      if (toDo.size == 0) update
      else loop(update.updated(toDo.head._1, update(toDo.head._1) + toDo.head._2), toDo.tail)
    }
    loop(current,toDo)
  }
  
  //=======================
  // Simulate baseline data
  
  // Calculate number of intervals in a year (e.g. 12 if looking monthly)
  val nIntervals: Int = if (period == "week") 52 else 12
  println("Time units are in " + period + "s")
  
  // Calculate number of data points in the baseline data
  val nData = (nYears*nIntervals).toInt
  println("No. of " + period + "s = " + nData)
  
  // Define function of j which appears in the sum from j= 1 to m:
  def sumTerm(j: Int): Double = gamma_1*math.cos(2*math.Pi*j*nYears) + gamma_2*math.sin(2*math.Pi*j*nYears)
		  
  // Calculate mean counts
  val mean_baseline = math.exp(alpha + beta*nData + sumFunction(sumTerm)(1,m))
  println("Mean counts of baseline data = " + mean_baseline)
  
  val rng = new PoissonDistribution(mean_baseline)
  val dataBaseline = (1 to nData).map(_ => rng.sample())
   
  //=======================
  // Simulate outbreak data
  
  // Define end of each period
  //Baseline -> Pre-outbreak -> Outbreak -> Post-outbreak  
  //val endBaseline = math.round(nData.toFloat / 3f)
  //val endPreOutbreak = math.round(nData.toFloat * 4f / 9f)
  //val endOutbreak = math.round(nData.toFloat * 2f / 3f)
  val endBaseline = 146
  val endPreOutbreak = 182
  val endOutbreak = 282
  println("Baseline period ends at " + period + " " + endBaseline)
  println("Per-outbreak period ends at " + period + " " + endPreOutbreak)
  println("Outbreak period ends at " + period + " " + endOutbreak)
      
  // Choose random time point in outbreak period at which to simulate outbreaks
  // tOutbreak = sample(periodOutbreak.head:periodOutbreak.last,nSimulations,replace=T)
  val rnd = new Random  
  val tOutbreak = (endPreOutbreak + 1) + rnd.nextInt(endOutbreak - endPreOutbreak)
  println("Outbreak occurs at " + period + " " + tOutbreak)  
  
  // Calculate standard deviation of the baseline count at each tOutbreak
  def stdDev(data: IndexedSeq[Int], mean: Double): Double = {
		  data.map(x => math.pow(x - mean, 2)).sum / nData
  }
  val sd = stdDev(dataBaseline, mean_baseline)
  println("Standard deviation of baseline data = " + sd)
    
  // Calculate no. of outbreak cases to simulate
  val rng_outbreak = new PoissonDistribution(k * sd)
  val nCases = rng_outbreak.sample()
  println("Number of outbreak cases = " + nCases)
  
  // Outbreak shape: Log normal with mean=0 and sd=0.5
  val outbreakShape = LogNormal(0,0.5).sample(nCases)
  //println(outbreakShape)
  
  // Distribute over required period (~3 months for short, ~6 months for long)
  val outbreakDistribution = 
    if (outbreakLength == "short") outbreakShape.sorted.map(x => math.floor(x).toInt)
    else outbreakShape.sorted.map(x => math.floor(2*x).toInt)
  //println(outbreakDistribution)
  
  // Count number of outbreak cases for each month of the outbreak
  val outbreakHist = outbreakDistribution.groupBy(w => w).mapValues(_.size).toList.sorted
  //println(outbreakHist)
  
  // Create list of pairs of time index and number of cases to be added
  val outbreakIdx = outbreakHist.map{case (key, value) => (key+tOutbreak-1, value)}
  //println(outbreakIdx)
  
  // Add to baseline data
  val dataOutbreak = addList(dataBaseline,outbreakIdx)
	
  //=======================
  // Output and plot
  
  val csvName = "results.csv"
  
  // Write to .csv file
  val writer = Files.newBufferedWriter(Paths.get(csvName), Charset.defaultCharset())
  writer.write("Baseline, Outbreak")
  writer.newLine
  dataBaseline.foreach{d => 
    writer.write(s"${d.toString}, ${dataOutbreak(d).toString}")
    writer.newLine   
  }
  writer.close
  
  // Export to R and plot
  val rScript = 
  s"""  
  setwd("ENVIRONMENT/workspaces/Sampler/sampler-spike")
  data = read.csv("$csvName")
  
  pdf("simulatedOutbreakData.pdf", width=4.13, height=2.91) #A7 landscape paper
  
  plot(1:nData,dataBaseline,"l",
     main = "Simulated baseline data",
     xlab = "Months",
     ylab = "No. of cases")

  plot(1:nData,dataOutbreak,"l",
       main = "Simulated outbreak data",
       xlab = "Months",
       ylab = "No. of cases")
  
  plot(1:nData,dataOutbreak-dataBaseline,"l",
       main = "Outbreak cases",
       xlab = "Months",
       ylab = "No. of cases")
  
  dev.off()
  """
   
  val wd = Paths.get("results").resolve("simulatedOutbreakData")
  
  println(wd)
  
  Files.createDirectories(wd)
  
  ScriptRunner.apply(rScript, wd.resolve("script.r"))
  
  
  
  
}