package farrington.study

import java.nio.file.Paths
import sampler.r.rserve.RServeHelper
import farrington.core.simulate.SimulateOutbreakData
import farrington.core.algorithm.Farrington
import farrington.core.algorithm.EDS
import sampler.r.process.ScriptRunner
import farrington.core.script.CreateRScript
import java.nio.file.Files
import java.nio.charset.Charset
import farrington.core.measures.AverageMeasures
import farrington.core.measures.Measures

object CompareEDS extends App {
  
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
  
  val magnitude = 0.6
  
  // Identifiers for results files
  val csv_Stats = "compareStats.csv" // CSV file to store simulated data from Scala
  val scriptName_Stats = "compareStats.r" // R script to import the CSV and plot the data
  
  // CSV file to store time to detection data
  val csv_APHA = "ttd_apha.csv"
  val csv_FarNew = "ttd_farNew.csv"
  val csv_Stl = "ttd_stl.csv"
  
  // R script to import the CSV and plot the data
  val script_APHA = "ttd_apha.r"
  val script_FarNew = "ttd_apha.r"
  val script_Stl = "ttd_apha.r"
  
  // PDFs containing the plots
  val pdf_APHA = "ttd_apha.pdf"
  val pdf_FarNew = "ttd_farNew.pdf"
  val pdf_Stl = "ttd_stl.pdf"
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "compareFarrington")
  
  //=======================
  // Simulation
  
  RServeHelper.ensureRunning()
  val measures_apha = (0 until nSimulations).par.map{ i =>
    println(i)
    val data = SimulateOutbreakData.run(nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)        
    val EDS_APHA = EDS.run(data, endBaseline, Farrington.APHA)
    Measures.allMeasures(EDS_APHA, data.start, data.end)
  }.toIndexedSeq
  RServeHelper.shutdown
  
  RServeHelper.ensureRunning()
  val measures_farNew = (0 until nSimulations).par.map{ i =>
    println(i)    
    val data = SimulateOutbreakData.run(nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
    val EDS_FarNew = EDS.run(data, endBaseline, Farrington.FarNew)
    Measures.allMeasures(EDS_FarNew, data.start, data.end)
  }.toIndexedSeq
  RServeHelper.shutdown
  
  RServeHelper.ensureRunning()
  val measures_stl = (0 until nSimulations).par.map{ i =>
    println(i)    
    val data = SimulateOutbreakData.run(nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
    val EDS_Stl = EDS.run(data, endBaseline, Farrington.Stl)      
    Measures.allMeasures(EDS_Stl, data.start, data.end)
  }.toIndexedSeq
  RServeHelper.shutdown
  
  val avgMeasures_apha = AverageMeasures.calculate(measures_apha, nSimulations)
  val avgMeasures_farNew = AverageMeasures.calculate(measures_farNew, nSimulations) 
  val avgMeasures_stl = AverageMeasures.calculate(measures_stl, nSimulations) 
  
  //=======================
  // Output and plot: Sensitivity and specificity measures  
      
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  // Write times to detection to CSV file for APHA
  val writerStats = Files.newBufferedWriter(resultsDir.resolve(csv_Stats), Charset.defaultCharset())
  writerStats.write("mode, pod, pocd, fpr, fprc, ppv, ppvc, ttd, ttcd, potd")
  writerStats.newLine
  writerStats.write("APHA," + AverageMeasures.toString(avgMeasures_apha))
  writerStats.newLine
  writerStats.write("FarringtonNew," + AverageMeasures.toString(avgMeasures_farNew))
  writerStats.newLine
  writerStats.write("Stl," + AverageMeasures.toString(avgMeasures_stl))
  writerStats.newLine
  writerStats.close
  
  // Write R script which imports and outputs table (html)
  val rScript_stats = CreateRScript.statsToTable(csv_Stats)
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript_stats, resultsDir.resolve(scriptName_Stats))
  
  //=======================
  // Output and plot: Time to detection
  
  // Create histogram data for time to detection in form of List(time, count)
  val TTD_APHA = measures_apha.map(_.TTD.head).groupBy{ x => x }.mapValues(_.size).toList.sorted
  val TTD_FarNew = measures_farNew.map(_.TTD.head).groupBy{ x => x }.mapValues(_.size).toList.sorted
  val TTD_Stl = measures_stl.map(_.TTD.head).groupBy{ x => x }.mapValues(_.size).toList.sorted
  
  // Write times to detection to CSV file for each mode
  Measures.writeTTD(TTD_APHA, resultsDir, csv_APHA)
  Measures.writeTTD(TTD_FarNew, resultsDir, csv_FarNew)
  Measures.writeTTD(TTD_Stl, resultsDir, csv_Stl)
  
  // Write R script which imports and plots data in a pdf
  val rScript_apha = CreateRScript.plotTTD(csv_APHA, pdf_APHA)
  val rScript_farNew = CreateRScript.plotTTD(csv_FarNew, pdf_FarNew)
  val rScript_stl = CreateRScript.plotTTD(csv_Stl, pdf_Stl)
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript_apha, resultsDir.resolve(script_APHA))
  ScriptRunner.apply(rScript_farNew, resultsDir.resolve(script_FarNew))
  ScriptRunner.apply(rScript_stl, resultsDir.resolve(script_Stl))
  
  //=======================
  // Output and plot: Time to consecutive detection
  
  // Write times to detection to CSV file for APHA
  val writer21 = Files.newBufferedWriter(resultsDir.resolve(csv_APHA_Con), Charset.defaultCharset())
  writer21.write("time, count")
  writer21.newLine
  for (i <- 0 until TTCD_APHA.length) {
    writer21.write(s"${TTCD_APHA(i)._1.toString}, ${TTCD_APHA(i)._2.toString}")
    writer21.newLine
  }
  writer21.close
  
  // Write times to detection to CSV file for Farrington New
  val writer22 = Files.newBufferedWriter(resultsDir.resolve(csv_FarNew_Con), Charset.defaultCharset())
  writer22.write("time, count")
  writer22.newLine
  for (i <- 0 until TTCD_FarNew.length) {
    writer22.write(s"${TTCD_FarNew(i)._1.toString}, ${TTCD_FarNew(i)._2.toString}")
    writer22.newLine
  }
  writer22.close
  
  // Write times to detection to CSV file for Stl
  val writer23 = Files.newBufferedWriter(resultsDir.resolve(csv_Stl_Con), Charset.defaultCharset())
  writer23.write("time, count")
  writer23.newLine
  for (i <- 0 until TTCD_Stl.length) {
    writer23.write(s"${TTCD_Stl(i)._1.toString}, ${TTCD_Stl(i)._2.toString}")
    writer23.newLine
  }
  writer23.close
  
  // Write R script which imports and plots data in a pdf
  val rScript_Con = 
    s"""
      
    APHA = read.csv("$csv_APHA_Con")
    FarNew = read.csv("$csv_FarNew_Con")
    Stl = read.csv("$csv_Stl_Con")
          
    cmin = 0
    cmax = max(APHA[["count"]], FarNew[["count"]], Stl[["count"]])
    
    pdf("$pdfName_Con", width=4.13, height=2.91) #A7 landscape paper
    
    barplot(APHA[["count"]],
          names.arg = as.character(APHA[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (APHA)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
          
    barplot(FarNew[["count"]],
          names.arg = as.character(FarNew[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (FarNew)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
          
    barplot(Stl[["count"]],
          names.arg = as.character(Stl[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (Stl)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
    
    dev.off()
    """
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript_Con, resultsDir.resolve(scriptName_Con))

}