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
import sampler.r.script.RScript

/*
  =========
  NOTES:
  Script which simulates a number of simulated outbreak data sets
  and splits the outbreak for each set between two setse.
  The time to detect the outbreak is calculated for each simulation
  and the success rate for the full outbreak and each of the separated
  outbreaks is calculated.
     
  Uses default parameters to simulate baseline and outbreak data
  (Default is Scenario 14 in Noufaily et al., Statist. Med. 2013 (32) 1206-1222)
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon
  Date:      04/03/2015
  Last edit: 10/03/2015
  
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



object EDS_HiddenOutbreak extends App{
  
  //=======================
  // User-defined parameters
  
  // Number of sets of data to simulate
  val nSimulations = 250
  
  // Number of months for which to simulate data:
  val nData = 462
  val endYear = 2014 
  
  // Choose "short" or "long" outbreaks
  //val outbreakLength = "short"
  val outbreakLength = "long"
  
  // Choose log-Normal or epidemic curve outbreak
  //val outbreakShape = "logNormal"
  val outbreakShape = "epidemicCurve"
  
  // Define end of each period
  //Baseline -> Pre-outbreak -> Outbreak -> Post-outbreak
  val endBaseline = 146
  val endPreOutbreak = 182
  val endOutbreak = 282
  
  // Magnitude of outbreak
  val magnitude = 0.6
      
  // Identifiers for results files
  val csv_full = "hiddenOutbreak_full.csv" // CSV file to store simulated data from Scala
  val csv_split1 = "hiddenOutbreak_split1.csv"
  val csv_split2 = "hiddenOutbreak_split2.csv"
  val scriptName = "plotHiddenOutbreak.r" // R script to import the CSV and plot the data
  val pdfName = "hiddenOutbreak.pdf" // PDF containing the plots
  
  // Choose directory to place data and results
  val resultsDir = Paths.get("results", "hiddenOutbreak")
  
  //=======================
  // Simulate outbreak data and calculate measures
    
  // For multiple simulations:
  ///*

  RServeHelper.ensureRunning()
  val stats = (0 until nSimulations).par.map{i =>
  //val stats = (0 until nSimulations).map{i =>
    println(i)
    
    // Simulate two sets of baseline data and combine to create full set
    val dataBaseline1 = GenerateData.runBaseline(nData, endYear)
    val dataBaseline2 = GenerateData.runBaseline(nData, endYear)
    val year = dataBaseline1.year
    val month = dataBaseline1.month
    val mean1 = dataBaseline1.mean
    val mean2 = dataBaseline2.mean
    val baseline1 = dataBaseline1.baseline
    val baseline2 = dataBaseline2.baseline
    val baselineFull = baseline1.zip(baseline2).map(i => i._1 + i._2)
    
    val meanFull = (0 until mean1.length).map(i => mean1(i) + mean2(i))
    val dataBaselineFull = BaselineResult(year, month, baselineFull, meanFull)
    
    // Simulate an outbreak using the full set of baseline data
    val data = GenerateData.addOutbreak(
      dataBaselineFull, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude
    )
    
    // Split the outbreak and add to the two smaller sets of baseline data:
    val (outbreak1, outbreak2) = GenerateData.splitOutbreak(data.hist, data.start)
    val data1 = GenerationResult(
        year,
        month,
        baseline1,
        GenerateData.addList(baseline1, outbreak1),
        outbreak1.map{ case (key, value) => (key - data.start + 1, value) },
        data.start,
        data.end,
        data.min,
        data.max)
    val data2 = GenerationResult(
        year,
        month,
        baseline2,
        GenerateData.addList(baseline2, outbreak2),
        outbreak2.map{ case (key, value) => (key - data.start + 1, value) },
        data.start,
        data.end,
        data.min,
        data.max)    
    
    // Run EDS for each data set
    val dataFull = EDS.run(data, endBaseline)    
    val dataSplit1 = EDS.run(data1, endBaseline)    
    val dataSplit2 = EDS.run(data2, endBaseline)
    
    // Probability of detection
    val detectedFull = EDS.detected(dataFull, data.start, data.end)    
    val detectedSplit1 = EDS.detected(dataSplit1, data.start, data.end)    
    val detectedSplit2 = EDS.detected(dataSplit2, data.start, data.end)
    
    val POD = IndexedSeq(detectedFull, detectedSplit1, detectedSplit2)
    
    // Probability of consecutive detection
    val consecutiveFull = EDS.detectedConsecutive(dataFull, data.start, data.end)    
    val consecutiveSplit1 = EDS.detectedConsecutive(dataSplit1, data.start, data.end)    
    val consecutiveSplit2 = EDS.detectedConsecutive(dataSplit2, data.start, data.end)
    
    val POCD = IndexedSeq(consecutiveFull, consecutiveSplit1, consecutiveSplit2)
    
    // False Positive Rate
    val fprFull = EDS.falsePositiveRate(dataFull, data.start, data.end)
    val fprSplit1 = EDS.falsePositiveRate(dataSplit1, data.start, data.end)
    val fprSplit2 = EDS.falsePositiveRate(dataSplit2, data.start, data.end)
    
    val FPR = IndexedSeq(fprFull, fprSplit1, fprSplit2)
    
    // False Positive Rate (consecutive)
    val fprFullCon = EDS.fprConsecutive(dataFull, data.start, data.end)
    val fprSplit1Con = EDS.fprConsecutive(dataSplit1, data.start, data.end)
    val fprSplit2Con = EDS.fprConsecutive(dataSplit2, data.start, data.end)
    
    val FPRCon = IndexedSeq(fprFullCon, fprSplit1Con, fprSplit2Con)
    
    // Positive predictive value
    val ppvFull = EDS.positivePredictive(dataFull, data.start, data.end)
    val ppvSplit1 = EDS.positivePredictive(dataSplit1, data.start, data.end)
    val ppvSplit2 = EDS.positivePredictive(dataSplit2, data.start, data.end)
    
    val PPV = IndexedSeq(ppvFull, ppvSplit1, ppvSplit2)
    
    // Positive predictive value
    val ppvFullCon = EDS.ppvConsecutive(dataFull, data.start, data.end)
    val ppvSplit1Con = EDS.ppvConsecutive(dataSplit1, data.start, data.end)
    val ppvSplit2Con = EDS.ppvConsecutive(dataSplit2, data.start, data.end)
    
    val PPVCon = IndexedSeq(ppvFullCon, ppvSplit1Con, ppvSplit2Con)
    
    // Time To Detection
    val timesFull = EDS.timeToDetection(dataFull, data.start, data.end)
    val tFull = if (timesFull.length == 0) -1 else timesFull(0)
    
    val timesSplit1 = EDS.timeToDetection(dataSplit1, data.start, data.end)
    val tSplit1 = if (timesSplit1.length == 0) -1 else timesSplit1(0)
    
    val timesSplit2 = EDS.timeToDetection(dataSplit2, data.start, data.end)
    val tSplit2 = if (timesSplit2.length == 0) -1 else timesSplit2(0)
    
    val TTD = IndexedSeq(tFull, tSplit1, tSplit2)
    
    // Time To Consecutive Detection
    val timesFullCon = EDS.timeToConsecutiveDetection(dataFull, data.start, data.end)
    val tFullCon = if (timesFullCon.length == 0) -1 else timesFullCon(0)
    
    val timesSplit1Con = EDS.timeToConsecutiveDetection(dataSplit1, data.start, data.end)
    val tSplit1Con = if (timesSplit1Con.length == 0) -1 else timesSplit1Con(0)
    
    val timesSplit2Con = EDS.timeToConsecutiveDetection(dataSplit2, data.start, data.end)
    val tSplit2Con = if (timesSplit2Con.length == 0) -1 else timesSplit2Con(0)
    
    val TTCD = IndexedSeq(tFullCon, tSplit1Con, tSplit2Con)
    
    // Proportion of Outbreak Times Detected
    val potdFull = EDS.proportionDetected(dataFull, data.start, data.end)    
    val potdSplit1 = EDS.proportionDetected(dataSplit1, data.start, data.end)    
    val potdSplit2 = EDS.proportionDetected(dataSplit2, data.start, data.end)
    
    val POTD = IndexedSeq(potdFull, potdSplit1, potdSplit2)
    MeasureData(POD, POCD, FPR, FPRCon, PPV, PPVCon, TTD, TTCD, POTD)
    
  }  
  RServeHelper.shutdown
  
  //=======================
  // Extract measures
  
  println("Magnitude of outbreak = " + magnitude)
    
  // Probability of detection
  val POD_Full =
    stats.map(i => i.POD(0)).count(i => i==true).toDouble / nSimulations
  val POD_Split1 =
    stats.map(i => i.POD(1)).count(i => i==true).toDouble / nSimulations
  val POD_Split2 =
    stats.map(i => i.POD(2)).count(i => i==true).toDouble / nSimulations
  
  val POD_Split = 1 - ((1 - POD_Split1) * (1 - POD_Split2))  
  
  // Probability of consecutive detection
  val POCD_Full =
    stats.map(i => i.POCD(0)).count(i => i==true).toDouble / nSimulations
  val POCD_Split1 =
    stats.map(i => i.POCD(1)).count(i => i==true).toDouble / nSimulations
  val POCD_Split2 =
    stats.map(i => i.POCD(2)).count(i => i==true).toDouble / nSimulations
  
  val POCD_Split = 1 - ((1 - POCD_Split1) * (1 - POCD_Split2))  
    
  // False positive rate
  val FPR_Full = stats.map(i => i.FPR(0)).sum.toDouble / nSimulations
  val FPR_Split1 = stats.map(i => i.FPR(1)).sum.toDouble / nSimulations
  val FPR_Split2 = stats.map(i => i.FPR(2)).sum.toDouble / nSimulations
  
  // False positive rate (consecutive
  val FPRCon_Full = stats.map(i => i.FPRCon(0)).sum.toDouble / nSimulations
  val FPRCon_Split1 = stats.map(i => i.FPRCon(1)).sum.toDouble / nSimulations
  val FPRCon_Split2 = stats.map(i => i.FPRCon(2)).sum.toDouble / nSimulations
  
  // False positive rate
  val PPV_Full = stats.map(i => i.PPV(0)).sum.toDouble / nSimulations
  val PPV_Split1 = stats.map(i => i.PPV(1)).sum.toDouble / nSimulations
  val PPV_Split2 = stats.map(i => i.PPV(2)).sum.toDouble / nSimulations
  
  // False positive rate (consecutive
  val PPVCon_Full = stats.map(i => i.PPVCon(0)).sum.toDouble / nSimulations
  val PPVCon_Split1 = stats.map(i => i.PPVCon(1)).sum.toDouble / nSimulations
  val PPVCon_Split2 = stats.map(i => i.PPVCon(2)).sum.toDouble / nSimulations
  
  // Time to detection
  val TTD_Full = 
    stats.map(i => i.TTD(0)).groupBy(w => w).mapValues(_.size).toList.sorted
  val TTD_Split1 = 
    stats.map(i => i.TTD(1)).groupBy(w => w).mapValues(_.size).toList.sorted
  val TTD_Split2 = 
    stats.map(i => i.TTD(2)).groupBy(w => w).mapValues(_.size).toList.sorted
    
  // Proportion of outbreak times detected
  val POTD_Full = stats.map(i => i.POTD(0)).sum.toDouble / nSimulations
  val POTD_Split1 = stats.map(i => i.POTD(1)).sum.toDouble / nSimulations
  val POTD_Split2 = stats.map(i => i.POTD(2)).sum.toDouble / nSimulations
  
  
  //=======================
  // Print relevant information to console
  
  println("Probability of any detection for full data = " + POD_Full)
  println("Probability of any detection for split 1 data = " + POD_Split1)
  println("Probability of any detection for split 2 data = " + POD_Split2)
  println("Probability of any detection when split = " + POD_Split)
  
  println("Probability of consecutive detection for full data = " + POCD_Full)
  println("Probability of consecutive detection for split 1 data = " + POCD_Split1)
  println("Probability of consecutive detection for split 2 data = " + POCD_Split2)
  println("Probability of consecutive detection when split = " + POCD_Split)
  
  println("False positive rate for full data = " + FPR_Full)
  println("False positive rate for split 1 data = " + FPR_Split1)
  println("False positive rate for split 2 data = " + FPR_Split2)
  
  println("False positive rate (consecutive) for full data = " + FPRCon_Full)
  println("False positive rate (consecutive) for split 1 data = " + FPRCon_Split1)
  println("False positive rate (consecutive) for split 2 data = " + FPRCon_Split2)
  
  println("Positive prediction for full data = " + PPV_Full)
  println("Positive prediction for split 1 data = " + PPV_Split1)
  println("Positive prediction for split 2 data = " + PPV_Split2)
  
  println("Positive prediction (consecutive) for full data = " + PPVCon_Full)
  println("Positive prediction (consecutive) for split 1 data = " + PPVCon_Split1)
  println("Positive prediction (consecutive) for split 2 data = " + PPVCon_Split2)
    
  println("Proportion of outbreak times detected for full data = " + POTD_Full)
  println("Proportion of outbreak times detected for split 1 data = " + POTD_Split1)
  println("Proportion of outbreak times detected for split 2 data = " + POTD_Split2)
  
  println("Time to detection saved to " + pdfName)
  
  //=======================
  // Output and plot
      
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  // Write times to detection to CSV file for full data
  val writer = Files.newBufferedWriter(resultsDir.resolve(csv_full), Charset.defaultCharset())
  writer.write("time, count")
  writer.newLine
  for (i <- 0 until TTD_Full.length) {
    writer.write(s"${TTD_Full(i)._1.toString}, ${TTD_Full(i)._2.toString}")
    writer.newLine
  }
  writer.close
  
  // Write times to detection to CSV file for split 1 data
  val writer2 = Files.newBufferedWriter(resultsDir.resolve(csv_split1), Charset.defaultCharset())
  writer2.write("time, count")
  writer2.newLine
  for (i <- 0 until TTD_Split1.length) {
    writer2.write(s"${TTD_Split1(i)._1.toString}, ${TTD_Split1(i)._2.toString}")
    writer2.newLine
  }
  writer2.close
  
  // Write times to detection to CSV file for split 2 data
  val writer3 = Files.newBufferedWriter(resultsDir.resolve(csv_split2), Charset.defaultCharset())
  writer3.write("time, count")
  writer3.newLine
  for (i <- 0 until TTD_Split2.length) {
    writer3.write(s"${TTD_Split2(i)._1.toString}, ${TTD_Split2(i)._2.toString}")
    writer3.newLine
  }
  writer3.close
  
  // Write R script which imports and plots data in a pdf
  val rScript = 
    s"""
      
    full = read.csv("$csv_full")
    split1 = read.csv("$csv_split1")
    split2 = read.csv("$csv_split2")
          
    cmin = 0
    cmax = max(full[["count"]], split1[["count"]], split2[["count"]])
    
    pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
    
    barplot(full[["count"]],
          names.arg = as.character(full[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (full set)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
          
    barplot(split1[["count"]],
          names.arg = as.character(split1[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (split 1)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
          
    barplot(split2[["count"]],
          names.arg = as.character(split2[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (split 2)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
    
    dev.off()
    """
  
  // Run the script in R and save the resulting PDF in the results directory
  RScript(rScript, resultsDir.resolve(scriptName))

  //=======================
  // Function definitions
  
  def successRate(nTotal: Int, hist: List[(Int, Int)]) = {
    val nMisses =
      if (hist.count(i => i._1 == -1) == 0) 100
      else hist(0)._2
    ((nTotal - nMisses).toDouble / nTotal) * 100
  }

}