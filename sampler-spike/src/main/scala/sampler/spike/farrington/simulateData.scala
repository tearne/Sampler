package sampler.spike.farrington

import scala.util.Random
import breeze.stats.distributions.LogNormal
import scala.annotation.tailrec
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.Charset
import sampler.r.process.ScriptRunner
import breeze.stats.distributions.Poisson
import breeze.stats.distributions.NegativeBinomial


/*
  =========
  NOTES:
  Simulates outbreak data for the EDS using a negative binomial
  model with dispersion parameter >= 1
  (Note that a Poission distribution is used if dispersion parameter == 1)
  
  Files are saved to a .csv file and imported in R for plotting.
  The resulting plots are saved to a pdf.
  
  Follows the method outlined in
  Noufaily et al., Statist. Med. 2013 (32) 1206-1222
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon
  Date:      19/02/2015
  Last edit: 26/02/2015
  
  ==========
  USER-DEFINED PARAMETERS:

	nYears          No. of years for which to simulate data
	period          Intervals at which to simulate data ("weekly" or "monthly")
  
  outbreakLength  Length of outbreak ("short" or "long")
	
	alpha           
	beta            Linear trend
	m               Seasonality (0 = none, 1 = annual, 2 = biannual)
	gamma_1         Controls magnitude of peaks
	gamma_2         Controls magnitude of troughs
	dispersion      Dispersion parameter
  
  k               Parameter which controls the magnitude of the outbreak
               
  csvName         Name of CSV file to store simulated data from Scala          
  scriptName      Name of R script to import the CSV and plot the data
  pdfName         Name of PDF containing the plots
	
	=========
  FUNCTIONS:
  
  stdDev          Calculates standard deviation
  sumFunction     Performs sum of a function of an integer from a to b
  sumTerm         Function of j which is to be summed from j= 1 to m:
  addList         Adds a list of values at the specified indices to a given sequence
  
  =========  
	OUTPUTS:
		
	dataBaseline    Simulated baseline data
  dataOutbreak    Simulated data with outbreak
  outbreakIdx     List of outbreak data in form List(month, no. of cases)
  
  */

object simulateData  extends App {

  //=======================
  // User-defined parameters
  
  // Number of years for which to simulate data:
  val nYears = 38.5
  
  // Choose to simulate data each "week" or "month"
  // period = "week"  // Not compatible with EDS yet: choose period = "month"
  val period = "month"
  
  // Choose "short" or "long" outbreaks
  // outbreakLength = "short"
  val outbreakLength = "long"
  
  // Define end of each period
  //Baseline -> Pre-outbreak -> Outbreak -> Post-outbreak
  val endBaseline = 146
  val endPreOutbreak = 182
  val endOutbreak = 282
  
  // Set of parameters to control baseline data
  val alpha = 1.5
  val beta = 0
  val m = 1
  val gamma_1 = 0.2
  val gamma_2 = -0.4
  val dispersion = 1.0
  
  // Parameter to control magnitude of outbreak data
  val k = 10
  
  // Identifiers for results files
  val csvName = "simData.csv" // CSV file to store simulated data from Scala
  val scriptName = "plotSimData.r" // R script to import the CSV and plot the data
  val pdfName = "simulatedOutbreakData.pdf" // PDF containing the plots
  
  // Choose directory to place simulated data
  val resultsDir = Paths.get("results", "simulatedOutbreakData")
  
  //=======================
  // Function definitions
  
  // Calculates standard deviation
  def stdDev(data: IndexedSeq[Int], mean: Double): Double = {
    data.map(x => math.pow(x - mean, 2)).sum / nData
  }
  
  // Performs sum of a function of an integer from integer a to integer b
  def sumFunction(f: Int => Double)(a: Int, b: Int): Double = {
    @tailrec
    def loop(a: Int, acc: Double): Double = {
      if (a > b) acc
      else loop(a+1, acc + f(a))
    }
    loop(a,0)    
  }
  
  // Function of j which is to be summed from j= 1 to m:
  def sumTerm(j: Int): Double = {
    gamma_1*math.cos(2*math.Pi*j*nYears) +
    gamma_2*math.sin(2*math.Pi*j*nYears)
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
  
  //=======================
  // Calculate years and months to assign to data
  
  val endYear = 2014
  val startYear = math.round(endYear - nYears)
  
  // Calculate number of intervals in a year (e.g. 12 if looking monthly)
  val nIntervals: Int = if (period == "week") 52 else 12
  println("Time units are in " + period + "s")
  
  // Calculate number of data points
  val nData = (nYears*nIntervals).toInt
  println("No. of " + period + "s = " + nData)
   
  val month = (1 to nData).map(i => (i-1) % 12 + 1)
  
  val year = (1 to nData).map(i => startYear + ((i-1) / 12))
  println("Data starts at " + year(0) + "-" + month(0))
  
  //=======================
  // Simulate baseline data
  
  // Calculate mean counts
  val mean_baseline = math.exp(alpha + beta*nData + sumFunction(sumTerm)(1,m))
  //println("Mean counts of baseline data = " + mean_baseline)
  
  // Sample baseline data from a Poisson distribution
  // Poisson is used if dispersion = 1
  // If dispersion > 1 use a negative binomial
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
  // Simulate outbreak data
  
  println("Baseline period ends at " + period + " " + endBaseline)
  println("Per-outbreak period ends at " + period + " " + endPreOutbreak)
  println("Outbreak period ends at " + period + " " + endOutbreak)
      
  // Choose random time point in outbreak period at which to simulate outbreaks
  // tOutbreak = sample(periodOutbreak.head:periodOutbreak.last,nSimulations,replace=T)
  val rnd = new Random  
  val tOutbreak = (endPreOutbreak + 1) + rnd.nextInt(endOutbreak - endPreOutbreak)
  println("Outbreak period is " + year(endPreOutbreak) + "-" + month(endPreOutbreak) + " to " + year(endOutbreak) + "-" + month(endOutbreak))
  println("Outbreak occurs at " + period + " " + tOutbreak)
  println("Outbreak date = " + year(tOutbreak-1)+ "-" + month(tOutbreak-1))
  
  // Calculate standard deviation of the baseline count at each tOutbreak
  val sd = stdDev(dataBaseline, mean_baseline)
  println("Standard deviation of baseline data = " + sd)
    
  // Calculate no. of outbreak cases to simulate
  val poi_outbreak = new Poisson(k * sd)
  val nCases = poi_outbreak.sample()
  println("Number of outbreak cases = " + nCases)
  
  // Outbreak shape: Log normal with mean=0 and sd=0.5
  val outbreakShape = LogNormal(0,0.5).sample(nCases)
  //println(outbreakShape)
  
  // Distribute over required period (~3 months for short, ~6 months for long)
  val outbreakDistribution = 
    if (outbreakLength == "short")
      outbreakShape.sorted.map(x => math.floor(x).toInt)
    else
      outbreakShape.sorted.map(x => math.floor(2*x).toInt)
  //println(outbreakDistribution)
  
  // Count number of outbreak cases for each month of the outbreak
  val outbreakHist = outbreakDistribution.groupBy(w => w).mapValues(_.size).toList.sorted
  //println(outbreakHist)
  
  // Create list of pairs of time index and number of cases to be added
  val outbreakIdx = outbreakHist.map{case (key, value) => (key+tOutbreak-1, value)}
  //println(outbreakIdx)
  
  // Add to baseline data
  val dataOutbreak = addList(dataBaseline,outbreakIdx)
  
  val tEnd = tOutbreak + outbreakIdx.length
  
  //=======================
  // Output and plot
  
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  //println(dataBaseline)
  //println(dataOutbreak)
   
  // Write baseline and outbreak data to CSV file
  val writer = Files.newBufferedWriter(resultsDir.resolve(csvName), Charset.defaultCharset())
  //writer.write("Year, Month, Baseline, Outbreak")
  //writer.newLine
  for (i <- 0 until nData) {
    writer.write(s"${year(i).toString}, ${month(i).toString}, ${dataBaseline(i).toString}, ${dataOutbreak(i).toString}")
    writer.newLine
  }
  writer.close
  
  // Write R script which imports and plots data in a pdf
  val rScript = 
  s"""  
    
  data = read.csv("$csvName")
  
  dataBaseline = data[[3]]
  dataOutbreak = data[[4]]
  
  nData = length(dataBaseline)
  
  cmin = min(c(dataBaseline,dataOutbreak))
  cmax = max(c(dataBaseline,dataOutbreak))
  
  pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
  
  plot(1:nData,dataBaseline,"l",
    ylim = c(cmin,cmax),
    main = "Simulated baseline data",
    xlab = "Months",
    ylab = "No. of cases")

  plot(1:nData,dataOutbreak,"l",
    ylim = c(cmin,cmax),
    main = "Simulated outbreak data",
    xlab = "Months",
    ylab = "No. of cases")
  
  outbreakCases = dataOutbreak - dataBaseline
  barplot(outbreakCases[$tOutbreak:$tEnd],
    names.arg=as.character(c($tOutbreak:$tEnd)),
    main = "Outbreak cases",
    xlab = "Months",
    ylab = "No. of cases")
  
  dev.off()
  """
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))


}