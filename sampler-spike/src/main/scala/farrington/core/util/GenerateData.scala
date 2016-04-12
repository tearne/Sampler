package farrington.core.util

import farrington.core.algorithm.Farrington
import java.nio.file.Paths
import farrington.core.simulate.SimulateOutbreakData
import farrington.core.simulate.OutbreakData
import sampler.r.process.ScriptRunner

object GenerateData extends App {
  
  //=======================
  // User-defined parameters
  
  // Number of months for which to simulate data:
  val nData = 462
  val endYear = 2014 
  
  // Choose "short" or "long" outbreaks
  // outbreakLength = "short"
  val outbreakLength = "long"
  
  // Choose log-Normal or epidemic curve outbreak
  // val outbreakShape = "logNormal"
  val outbreakShape = "epidemicCurve"
  
  // Define end of each period
  //Baseline -> Pre-outbreak -> Outbreak -> Post-outbreak
  val endBaseline = 146
  val endPreOutbreak = 182
  val endOutbreak = 282
  
  val mode = Farrington.APHA
//  val mode = Farrington.FarNew
//  val mode = Farrington.Stl
  
  // Identifiers for results files
  val csvName = "simData.csv" // CSV file to store simulated data from Scala
  val scriptName = "plotSimData.r" // R script to import the CSV and plot the data
  val pdfName = "simulatedOutbreakData.pdf" // PDF containing the plots
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "simulatedOutbreakData")
  
  //=======================
  // Simulate and write to file
  
  val data = SimulateOutbreakData.run(nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, 1)
  
  OutbreakData.writeToFile(data, resultsDir, csvName)
  
  //=======================
  // Write R script to plot data in pdf
  val rScript = 
    s"""
      
    data = read.csv("$csvName")
      
    month = data[["month"]]
    dataBaseline = data[["baseline"]]
    dataOutbreak = data[["outbreak"]]
    start = data[["start"]][1]
    end = data[["end"]][1]
    
    counts = dataOutbreak - dataBaseline
    
    pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
      
    cmin = min(c(dataBaseline,dataOutbreak))
    cmax = max(c(dataBaseline,dataOutbreak))
    
    plot(month,dataBaseline,"l",
         ylim = c(cmin,cmax),
         main = "Simulated baseline data",
         xlab = "Months",
         ylab = "No. of cases")
    
    plot(month,dataOutbreak,"l",
         ylim = c(cmin,cmax),
         main = "Simulated outbreak data",
         xlab = "Months",
         ylab = "No. of cases")
    
    barplot(counts[c(start:end)],
          names.arg=as.character(month[c(start:end)]),
          main = "Outbreak",
          xlab = "Month",
          ylab = "No. of outbreak cases")
    
    dev.off()
    """
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))

}