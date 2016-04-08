package farrington.study

import java.nio.file.Paths
import sampler.r.rserve.RServeHelper
import org.rosuda.REngine.Rserve.RConnection
import farrington.core.algorithm.EDS
import farrington.core.algorithm.Farrington
import farrington.core.simulate.SimulateOutbreakData
import sampler.r.process.ScriptRunner
import java.nio.file.Files
import farrington.core.script.CreateRScript
import java.nio.charset.Charset
import farrington.core.measures.AverageMeasures
import farrington.core.measures.Measures

object CompareAll extends App {
  
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
  
  val csv_APHA = "compareTTD_All_APHA.csv" // CSV file to store data from Scala
  val csv_FarNew = "compareTTD_All_FarNew.csv"
  val csv_Stl = "compareTTD_All_Stl.csv"
  
  val csv_APHA_Con = "compareTTD_Con_APHA.csv" // CSV file to store data from Scala
  val csv_FarNew_Con = "compareTTD_Con_FarNew.csv"
  val csv_Stl_Con = "compareTTD_Con_Stl.csv"
  
  val scriptName = "compareTTD_All.r" // R script to import the CSV and plot the data
  val pdfName = "compareTTD_All.pdf" // PDF containing the plots
    
  val scriptName_Con = "compareTTD_Con.r" // R script to import the CSV and plot the data
  val pdfName_Con = "compareTTD_Con.pdf" // PDF containing the plots
  
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
  
  val TTD_APHA = measures_apha.map(_.TTD).groupBy{ x => x }.mapValues(_.size).toList.sorted
  val TTD_FarNew = measures_farNew.map(_.TTD).groupBy{ x => x }.mapValues(_.size).toList.sorted
  val TTD_Stl = measures_stl.map(_.TTD).groupBy{ x => x }.mapValues(_.size).toList.sorted
  
  // Write times to detection to CSV file for APHA
  val writer = Files.newBufferedWriter(resultsDir.resolve(csv_APHA), Charset.defaultCharset())
  writer.write("time, count")
  writer.newLine
  for (i <- 0 until TTD_APHA.length) {
    writer.write(s"${TTD_APHA(i)._1.toString}, ${TTD_APHA(i)._2.toString}")
    writer.newLine
  }
  writer.close
  
  // Write times to detection to CSV file for Farrington New
  val writer2 = Files.newBufferedWriter(resultsDir.resolve(csv_FarNew), Charset.defaultCharset())
  writer2.write("time, count")
  writer2.newLine
  for (i <- 0 until TTD_FarNew.length) {
    writer2.write(s"${TTD_FarNew(i)._1.toString}, ${TTD_FarNew(i)._2.toString}")
    writer2.newLine
  }
  writer2.close
  
  // Write times to detection to CSV file for Stl
  val writer3 = Files.newBufferedWriter(resultsDir.resolve(csv_Stl), Charset.defaultCharset())
  writer3.write("time, count")
  writer3.newLine
  for (i <- 0 until TTD_Stl.length) {
    writer3.write(s"${TTD_Stl(i)._1.toString}, ${TTD_Stl(i)._2.toString}")
    writer3.newLine
  }
  writer3.close
  
  // Write R script which imports and plots data in a pdf
  val rScript = 
    s"""
      
    APHA = read.csv("$csv_APHA")
    FarNew = read.csv("$csv_FarNew")
    Stl = read.csv("$csv_Stl")
          
    cmin = 0
    cmax = max(APHA[["count"]], FarNew[["count"]], Stl[["count"]])
    
    pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
    
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
  ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))
  
  //=======================
  // Output and plot: Time to detection
  
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