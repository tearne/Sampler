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
import sampler.spike.farrington.Farrington.APHA

/*
  =========
  NOTES:
  Simulate outbreak data and run an Early Detection System,
  which uses the Farrington algorithm to calculate the maximum number
  of outbreak cases that should be expected each month.  
  
  Follows the method outlined in Farrington et al., 1996
  
  Uses default parameters to simulate baseline and outbreak data
  (Scenario 14 in Noufaily et al., Statist. Med. 2013 (32) 1206-1222)
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon (modified from EDS.scala by Oliver Tearne)
  Date:      26/02/2015
  Last edit: 10/03/2015
  
  ==========
  USER-DEFINED PARAMETERS:

  nData           No. of months for which to simulate data
  
  outbreakLength  Length of outbreak ("short" or "long")
               
  endBaseline     Month in which baseline period ends
  endPreOutbreak  Month in which pre-outbreak period ends
  endOutbreak     Month in which outbreak period ends
  
  =========  
  OUTPUTS:
    
  date
  actual
  expected
  threshold
  trend
  exceed
  weights
  isAlert
  
  
  */

object EDS_simData extends App{
  
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
  
  val magnitude = 8
  
//  val mode = Farrington.APHA
  val mode = Farrington.FarNew
//  val mode = Farrington.Stl
  
  // Identifiers for results files
  val csvName = "simData.csv" // CSV file to store simulated data from Scala
  val scriptName = "plotSimData.r" // R script to import the CSV and plot the data
  val pdfName = "simulatedOutbreakData.pdf" // PDF containing the plots
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "simulatedOutbreakData")
  
  //=======================
  // Simulate outbreak data
  
  val data = GenerateData.run(
      nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
  
  //RServeHelper.shutdown
  RServeHelper.ensureRunning()
  val EDS_result = EDS.run(data, endBaseline, mode)
  RServeHelper.shutdown
  
  val results = EDS_result.results
  
  // Probability of detection
  val POD = EDS.detected(EDS_result, data.start, data.end)

  // Probability of consecutive detection
  val POCD = EDS.detectedConsecutive(EDS_result, data.start, data.end)    
  
  // False Positive Rate
  val FPR = EDS.falsePositiveRate(EDS_result, data.start, data.end)
  
  // True negative rate
  val TNR = EDS.trueNegativeRate(EDS_result, data.start, data.end)
  
  // Positive predictive value
  val PPV = EDS.positivePredictive(EDS_result, data.start, data.end)
  
  // False Positive Rate
  val FPRcon = EDS.fprConsecutive(EDS_result, data.start, data.end)
  
  // True negative rate
  val TNRcon = EDS.tnrConsecutive(EDS_result, data.start, data.end)
  
  // Positive predictive value
  val PPVcon = EDS.ppvConsecutive(EDS_result, data.start, data.end)

  // Time To Detection
  val times = EDS.timeToDetection(EDS_result, data.start, data.end)
  val TTD = if (times.length == 0) -1 else times(0)
    
  // Proportion of Outbreak Times Detected
  val POTD = EDS.proportionDetected(EDS_result, data.start, data.end)
  
  //=======================
  // Print relevant information to console:
  
  println("Total no. of months = " + nData)
  
  println("Baseline period starts at " + 1 + " = " + data.year(0) + "-" + data.month(0))
  println("Pre-outbreak period starts at " + (endBaseline + 1) + " = " + data.year(endBaseline) + "-" + data.month(endBaseline))
  println("Outbreak period starts at " + (endPreOutbreak + 1) + " = " + data.year(endPreOutbreak) + "-" + data.month(endPreOutbreak))
  println("Post-outbreak period starts at " + (endOutbreak + 1) + " = " + data.year(endOutbreak) + "-" + data.month(endOutbreak))
  
  println("Outbreak begins at month " + data.start + " = " + data.year(data.start-1) + "-" + data.month(data.start-1))
  println("Outbreak occurs during months " + data.start + "-" + data.end)
  println("Outbreak counts " + data.hist)
  
  println("Outbreak detected = " + POD)
  println("Outbreak detected (consecutive) = " + POCD)
  println("False positive rate = " + FPR)
  println("False positive rate (consecutive) = " + FPRcon)
  println("True negative rate = " + TNR)
  println("True negative rate (consecutive) = " + TNRcon)
  println("Positive predictive value = " + PPV)
  println("Positive predictive value (consecutive) = " + PPVcon)
  println("Time to detection = " + TTD)
  println("Proportion of outbreak times detected = " + POTD)
  
  //=======================
  // Visualisation: Time to detection
  
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  // Write times to CSV file
  val writer = Files.newBufferedWriter(resultsDir.resolve(csvName), Charset.defaultCharset())
  writer.write("month, baseline, outbreak, start, end")
  writer.newLine
  writer.write(s"${1.toString}, ${data.baseline(0).toString}, ${data.counts(0).toString}, ${data.start.toString}, ${data.end.toString}")
  writer.newLine
  for (i <- 1 until nData) {
    writer.write(s"${(i+1).toString}, ${data.baseline(i).toString}, ${data.counts(i).toString}")
    writer.newLine
  }
  writer.close
  
  // Write R script which imports and plots data in a pdf
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
  
  //=======================
  // Visualisation: EDS
   
  val csvName2 = "EDSoutput.csv"
  val scriptName2 = "EDSoutput.r"
  val pdfName2 = "EDSoutput.pdf"
  
  // Write times to CSV file
  val writerEDS = Files.newBufferedWriter(resultsDir.resolve(csvName2), Charset.defaultCharset())
  writerEDS.write("month, count, threshold")
  writerEDS.newLine
  for (i <- 0 until results.threshold.length) {
    writerEDS.write(s"${results.date(i).idx.toString}, ${results.actual(i).toString}, ${results.threshold(i).toString}")
    writerEDS.newLine
  }
  writerEDS.close
  
  val rScript2 = 
    s"""
      
    data = read.csv("$csvName2")

    month = data[["month"]]
    count = data[["count"]]
    thresh = data[["threshold"]]
    
    pdf("$pdfName2", width=8.27, height=5.83) #A5 landscape paper
    
    eds <- barplot(count,
          names.arg=as.character(month),
          main = "EDS results",
          xlab = "Time (months)",
          ylab = "No. of cases")
    lines(x = eds, y = thresh, type="l", col="red")
    
    dev.off()
    """
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript2, resultsDir.resolve(scriptName2)) 
  
  val monthStr = results.date.map(_.yearMonth.toString)
  val idx = results.date.map(_.idx)
  
  val timeSeriesJSON = 
    ("source" -> "Simulated data" ) ~
    ("month" -> monthStr) ~
    ("monthId" -> idx) ~
    ("expected" -> results.expected) ~
    ("threshold" -> results.threshold) ~
    ("actual" -> results.actual)
  //println(timeSeriesJSON)
  
  // Create html plot
  FreeMarkerHelper.writeFile(
    Map("jsonData" -> pretty(render(timeSeriesJSON))),
    "plot.ftl",
    resultsDir.resolve("EDSoutput.html") 
  ) 
  
  
}