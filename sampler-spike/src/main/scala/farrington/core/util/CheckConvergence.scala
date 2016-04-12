package farrington.core.util

import sampler.r.rserve.RServeHelper
import org.rosuda.REngine.Rserve.RConnection
import java.nio.file.Paths
import java.nio.file.Files
import farrington.core.simulate.SimulateOutbreakData
import farrington.core.algorithm.EDS
import farrington.core.algorithm.Farrington
import farrington.core.measures.Measures
import java.nio.charset.Charset
import sampler.r.process.ScriptRunner

object CheckConvergence extends App {
  
  //=======================
  // User-defined parameters
  
  // Number of sets of data to simulate
  val nSimulations = 100
  
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
  
  val magnitude = 5
  
  // Identifiers for results files
  val csvName = "convergence.csv" // CSV file to store simulated data from Scala
  val scriptName = "convergence.r" // R script to import the CSV and plot the data
  val pdfName = "convergence.pdf" // PDF containing the plots
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "convergence")
  
  //=======================
  // Simulation and output
  
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  RServeHelper.ensureRunning()
  val PODpar = {
    val result = (0 until nSimulations).par.map{ i =>
      println(i)
      val data = SimulateOutbreakData.run(nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
      val EDS_APHA = EDS.run(data, endBaseline, Farrington.APHA)
      val detected_APHA = Measures.detected(EDS_APHA, data.start, data.end)
      if (detected_APHA) 1 else 0
    }
    (0 until nSimulations).map{ i => 
      println(i)
      result.take(i).sum.toDouble / (i+1)
    }
  }
  RServeHelper.shutdown
  
  val POD = PODpar.toIndexedSeq
  
  val writer = Files.newBufferedWriter(resultsDir.resolve(csvName), Charset.defaultCharset())
  for (i <- 0 until POD.length) {
    writer.write(s"${POD(i).toString}")
    writer.newLine
  }
  writer.close
  
  // Write R script which imports and outputs table (html)
  val rScript = 
    s"""      
    pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
    data = read.csv("$csvName")      
    pod = data[[1]]      
    n = length(pod)    
    plot(1:n, pod, type="l", xlab="Simulation number", ylab="Probability of detection")    
    dev.off()
    """
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))

}