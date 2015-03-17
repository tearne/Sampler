package sampler.spike.farrington

import java.nio.file.{Files,Paths}
import java.time.YearMonth
import scala.annotation.elidable.ASSERTION
import scala.collection.immutable.TreeMap
import scala.io.Source
import scala.language.{postfixOps,implicitConversions}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.rosuda.REngine.Rserve.RConnection
import sampler.r.rserve.RServeHelper
import java.time._
import java.time.Year
import java.time.ZoneId
import java.time.temporal.ChronoUnit._
import java.time.temporal.ChronoField
import java.time.Duration.of
import java.time.Duration
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalUnit
import scala.collection.SortedMap
import org.json4s.JsonAST.JValue
import org.json4s.native.JsonMethods
import java.nio.charset.Charset
import java.nio.file.Path
import java.io.OutputStream
import sampler.r.process.ScriptRunner

/*
  =========
  NOTES:
  Script which simulates a number of simulated outbreak data sets
  and tests each of the four variations to the Farrington algorithm
  using various measures
    - probability of detection
    - probability of consecutive detection
    - false positive rate
    - time to detection
    - proportion of outbreak times detected
  
  Uses default parameters to simulate baseline and outbreak data
  (Default is Scenario 14 in Noufaily et al., Statist. Med. 2013 (32) 1206-1222)
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon
  Date:      01/03/2015
  Last edit: 11/03/2015
  
  ==========
  USER-DEFINED PARAMETERS:

  nSimulations            Number of sets of simulated data to run
  nData                   Length of each data set (number of months)
  endYear                 Last year for which to simulate data
  outbreakLength          Length of the outbreak ("short" or "long")
  endBaseline             End month of baseline period
  endPreOutbreak          End month of pre-outbreak period
  endOutbreak             End of outbreak period
  
  csvName                 Name of csv file to store histogram data
  scriptName              Name of R script for plotting histograms
  pdfName                 Name of pdf of plots
  resultsDir              Name of directory where results will be saved
  
  =========
  FUNCTIONS:
  
  successRate             Calculates success of detection (%)
  
  =========  
  OUTPUTS:
    
  
  
  
  */

object EDS_TestFarringtonMethods extends App{
  
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
  val csv_Stats = "compareFarrington_stats.csv"
  val csv_APHA = "compareFarrington_APHA.csv" // CSV file to store simulated data from Scala
  val csv_FarNew = "compareFarrington_FarNew.csv"
  val csv_Stl = "compareFarrington_Stl.csv"
  val scriptName = "compareFarrington.r" // R script to import the CSV and plot the data
  val pdfName = "compareFarrington.pdf" // PDF containing the plots
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "compareFarrington")
  
  //=======================
  // Simulation
  
  RServeHelper.shutdown
  RServeHelper.ensureRunning()
  val stats = (0 until nSimulations).par.map{i =>
  // val stats = (0 until nSimulations).map{i =>
  println(i)
  
    val data = GenerateData.run(
      nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
      
    val EDS_APHA = EDS.run(data, endBaseline, Farrington.APHA)
    //println("Done APHA")
    val EDS_FarNew = EDS.run(data, endBaseline, Farrington.FarNew)
    //println("Done FarNew")
    val EDS_Stl = EDS.run(data, endBaseline, Farrington.Stl)
    //println("Done Stl")
        
    val results_APHA = EDS_APHA.results
    val results_FarNew = EDS_FarNew.results
    val results_Stl = EDS_Stl.results
    
    // Probability of detection
    val detected_APHA = EDS.detected(EDS_APHA, data.start, data.end)
    val detected_FarNew = EDS.detected(EDS_FarNew, data.start, data.end)
    val detected_Stl = EDS.detected(EDS_Stl, data.start, data.end)
        
    val POD = IndexedSeq(detected_APHA, detected_FarNew, detected_Stl)
    //println(POD)
  
    // Probability of consecutive detection
    val consecutive_APHA = EDS.detectedConsecutive(EDS_APHA, data.start, data.end)
    val consecutive_FarNew = EDS.detectedConsecutive(EDS_FarNew, data.start, data.end)
    val consecutive_Stl = EDS.detectedConsecutive(EDS_Stl, data.start, data.end)
  
    val POCD = IndexedSeq(consecutive_APHA, consecutive_FarNew, consecutive_Stl)
    //println(POCD)
    
    // False Positive Rate
    val FPR_APHA = EDS.falsePositiveRate(EDS_APHA, data.start, data.end)
    val FPR_FarNew = EDS.falsePositiveRate(EDS_FarNew, data.start, data.end)
    val FPR_Stl = EDS.falsePositiveRate(EDS_Stl, data.start, data.end)
    
    val FPR = IndexedSeq(FPR_APHA, FPR_FarNew, FPR_Stl)
    //println(FPR)
    
    // False Positive Rate (consecutive)
    val FPRCon_APHA = EDS.fprConsecutive(EDS_APHA, data.start, data.end)
    val FPRCon_FarNew = EDS.fprConsecutive(EDS_FarNew, data.start, data.end)
    val FPRCon_Stl = EDS.fprConsecutive(EDS_Stl, data.start, data.end)
    
    val FPRCon = IndexedSeq(FPR_APHA, FPR_FarNew, FPR_Stl)
    
    // Positive predictive value
    val ppvAPHA = EDS.positivePredictive(EDS_APHA, data.start, data.end)
    val ppvFarNew = EDS.positivePredictive(EDS_FarNew, data.start, data.end)
    val ppvStl = EDS.positivePredictive(EDS_Stl, data.start, data.end)
    
    val PPV = IndexedSeq(ppvAPHA, ppvFarNew, ppvStl)
    
    // Positive predictive value
    val ppvAPHACon = EDS.ppvConsecutive(EDS_APHA, data.start, data.end)
    val ppvFarNewCon = EDS.ppvConsecutive(EDS_FarNew, data.start, data.end)
    val ppvStlCon = EDS.ppvConsecutive(EDS_Stl, data.start, data.end)
    
    val PPVCon = IndexedSeq(ppvAPHACon, ppvFarNewCon, ppvStlCon)
  
    // Time To Detection  
    val times_APHA = EDS.timeToDetection(EDS_APHA, data.start, data.end)
    val times_FarNew = EDS.timeToDetection(EDS_FarNew, data.start, data.end)
    val times_Stl = EDS.timeToDetection(EDS_Stl, data.start, data.end)
    
    val TTD_APHA = if (times_APHA.length == 0) -1 else times_APHA(0)
    val TTD_FarNew = if (times_FarNew.length == 0) -1 else times_FarNew(0)
    val TTD_Stl = if (times_Stl.length == 0) -1 else times_Stl(0)
    
    val TTD = IndexedSeq(TTD_APHA, TTD_FarNew, TTD_Stl)
    //println(TTD)
    
    // Proportion of Outbreak Times Detected
    val POTD_APHA = EDS.proportionDetected(EDS_APHA, data.start, data.end)
    val POTD_FarNew = EDS.proportionDetected(EDS_FarNew, data.start, data.end)
    val POTD_Stl = EDS.proportionDetected(EDS_Stl, data.start, data.end)
    
    val POTD = IndexedSeq(POTD_APHA, POTD_FarNew, POTD_Stl)
    //println(POTD)
    
    MeasureData(POD, POCD, FPR, FPRCon, PPV, PPVCon, TTD, POTD)
    
  }
  RServeHelper.shutdown
    
  //=======================
  // Extract measures
     
  // Probability of detection
  val POD_APHA =
    stats.map(i => i.POD(0)).count(i => i==true).toDouble / nSimulations
  val POD_FarNew =
    stats.map(i => i.POD(1)).count(i => i==true).toDouble / nSimulations
  val POD_Stl =
    stats.map(i => i.POD(2)).count(i => i==true).toDouble / nSimulations
    
  // Probability of consecutive detection
  val POCD_APHA =
    stats.map(i => i.POCD(0)).count(i => i==true).toDouble / nSimulations
  val POCD_FarNew =
    stats.map(i => i.POCD(1)).count(i => i==true).toDouble / nSimulations
  val POCD_Stl =
    stats.map(i => i.POCD(2)).count(i => i==true).toDouble / nSimulations
    
  // False positive rate
  val FPR_APHA = stats.map(i => i.FPR(0)).sum.toDouble / nSimulations
  val FPR_FarNew = stats.map(i => i.FPR(1)).sum.toDouble / nSimulations
  val FPR_Stl = stats.map(i => i.FPR(2)).sum.toDouble / nSimulations
  
  // False positive rate
  val FPRCon_APHA = stats.map(i => i.FPRCon(0)).sum.toDouble / nSimulations
  val FPRCon_FarNew = stats.map(i => i.FPRCon(1)).sum.toDouble / nSimulations
  val FPRCon_Stl = stats.map(i => i.FPRCon(2)).sum.toDouble / nSimulations
  
  // Positive predictive value
  val PPV_APHA = stats.map(i => i.PPV(0)).sum.toDouble / nSimulations
  val PPV_FarNew = stats.map(i => i.PPV(1)).sum.toDouble / nSimulations
  val PPV_Stl = stats.map(i => i.PPV(2)).sum.toDouble / nSimulations
  
  // False positive rate
  val PPVCon_APHA = stats.map(i => i.PPVCon(0)).sum.toDouble / nSimulations
  val PPVCon_FarNew = stats.map(i => i.PPVCon(1)).sum.toDouble / nSimulations
  val PPVCon_Stl = stats.map(i => i.PPVCon(2)).sum.toDouble / nSimulations
  
  // Time to detection
  val TTD_APHA = 
    stats.map(i => i.TTD(0)).groupBy(w => w).mapValues(_.size).toList.sorted
  val TTD_FarNew = 
    stats.map(i => i.TTD(1)).groupBy(w => w).mapValues(_.size).toList.sorted
  val TTD_Stl = 
    stats.map(i => i.TTD(2)).groupBy(w => w).mapValues(_.size).toList.sorted
    
  // Proportion of outbreak times detected
  val POTD_APHA = stats.map(i => i.POTD(0)).sum.toDouble / nSimulations
  val POTD_FarNew = stats.map(i => i.POTD(1)).sum.toDouble / nSimulations
  val POTD_Stl = stats.map(i => i.POTD(2)).sum.toDouble / nSimulations
  
  
  //=======================
  // Print relevant information to console
  println("Magnitude of outbreak = " + magnitude)
  
  println("Probability of any detection for APHA = " + POD_APHA)
  println("Probability of any detection for Farrington new = " + POD_FarNew)
  println("Probability of any detection for Stl = " + POD_Stl)
  
  println("Probability of consecutive detection for APHA = " + POCD_APHA)
  println("Probability of consecutive detection for Farrington new = " + POCD_FarNew)
  println("Probability of consecutive detection for Stl = " + POCD_Stl)
  
  println("False positive rate for APHA = " + FPR_APHA)
  println("False positive rate for Farrington new= " + FPR_FarNew)
  println("False positive rate for Stl = " + FPR_Stl)
  
  println("False positive rate (consecutive) for APHA = " + FPRCon_APHA)
  println("False positive rate (consecutive) for Farrington new= " + FPRCon_FarNew)
  println("False positive rate (consecutive) for Stl = " + FPRCon_Stl)
  
  println("Positive predictive value for APHA = " + PPV_APHA)
  println("Positive predictive value for Farrington new= " + PPV_FarNew)
  println("Positive predictive value for Stl = " + PPV_Stl)
  
  println("Positive predictive value (consecutive) for APHA = " + PPVCon_APHA)
  println("Positive predictive value (consecutive) for Farrington new= " + PPVCon_FarNew)
  println("Positive predictive value (consecutive) for Stl = " + PPVCon_Stl)
    
  println("Proportion of outbreak times detected for APHA = " + POTD_APHA)
  println("Proportion of outbreak times detected for Farrington new = " + POTD_FarNew)
  println("Proportion of outbreak times detected for Stl = " + POTD_Stl)
  
  println("Time to detection saved to " + pdfName)
  
  //=======================
  // Output and plot
      
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  // Write times to detection to CSV file for APHA
  val writerStats = Files.newBufferedWriter(resultsDir.resolve(csv_Stats), Charset.defaultCharset())
  writerStats.write("mode, pod, pocd, fpr, fprc, ppv, ppvc, ttd, ttcd, notd")
  writerStats.newLine
  for (i <- 0 until 2) {
      writerStats.write(s" ${"APHA"}, ${POD_APHA.toString}, ${POCD_APHA.toString}, ${FPR_APHA.toString}, ${FPRCon_APHA.toString}, ${PPV_APHA.toString}, ${PPVCon_APHA.toString}, ${TTD_APHA.toString}, ${POTD_APHA.toString}")
      writerStats.newLine
    }
  writerStats.newLine
  writerStats.close
  
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

}