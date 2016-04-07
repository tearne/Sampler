package farrington

import sampler.r.process.ScriptRunner
import farrington.core.simulate.OutbreakData
import farrington.core.simulate.SimulateOutbreakData
import java.nio.file.Paths
import farrington.core.algorithm.Farrington
import farrington.core.script.CreateRScript
import farrington.core.algorithm.EDS
import java.nio.file.Files
import java.nio.charset.Charset
import sampler.r.rserve.RServeHelper
import org.rosuda.REngine.Rserve.RConnection

object Example extends App {
  
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
  
  // Identifiers for results files
  val csvSimData = "simData.csv" // CSV file to store simulated data from Scala
  val scriptSimData = "plotSimData.r" // R script to import the CSV and plot the data
  val pdfSimData = "simulatedOutbreakData.pdf" // PDF containing the plots
    
  val csvCompare = "compareEDS.csv"
  val scriptCompare = "compareEDS.r" // R script to import the CSV and plot the data
  val pdfCompare = "compareEDS.pdf" // PDF containing the plots
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "compareEDS")
  
  //=======================
  // Simulate outbreak data and write to file  
  val data = SimulateOutbreakData.run(nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, 1)  
  OutbreakData.writeToFile(data, resultsDir, csvSimData)
  println("Outbreak data written to " + csvSimData)
  
  //=======================
  // Create R script, run the script in R and save the resulting PDF in the results directory
  val rScript_simData = CreateRScript.plotSimulatedData(csvSimData, pdfSimData)
  ScriptRunner.apply(rScript_simData, resultsDir.resolve(scriptSimData))
  println("Outbreak data plots written to " + pdfSimData)
  
  //=======================
  // Run the Early Detection System for all modes (using defaults)
  
  // Run EDS for each mode
  RServeHelper.ensureRunning()
  val result_apha = EDS.run(data, endBaseline, Farrington.APHA)
  val result_farNew = EDS.run(data, endBaseline, Farrington.FarNew)
  val result_stl = EDS.run(data, endBaseline, Farrington.Stl)
  RServeHelper.shutdown
  
  Files.createDirectories(resultsDir)
  
  val numResults = Math.min(Math.min(result_apha.results.threshold.length, result_farNew.results.threshold.length), 100)
  val APHA_thresh = result_apha.results.threshold.takeRight(numResults)
  val FarNew_thresh = result_farNew.results.threshold.takeRight(numResults)
  val Stl_thresh = result_stl.results.threshold.takeRight(numResults)
  
  // Write times to CSV file
  val writerEDS = Files.newBufferedWriter(resultsDir.resolve(csvCompare), Charset.defaultCharset())
  writerEDS.write("month, count, APHA, FarNew, Stl")
  writerEDS.newLine
  for (i <- 0 until numResults) {
    writerEDS.write(s"${result_apha.results.date(i).idx.toString}, ${result_apha.results.actual(i).toString}, ${APHA_thresh(i).toString}, ${FarNew_thresh(i).toString}, ${Stl_thresh(i).toString}")
    writerEDS.newLine
  }
  writerEDS.close
  println("EDS results written to " + csvCompare)
  
  val rScript_comparison = CreateRScript.plotComparison(csvCompare, pdfCompare)
  ScriptRunner.apply(rScript_comparison, resultsDir.resolve(scriptCompare))
  println("EDS comparison plots written to " + pdfCompare)

}