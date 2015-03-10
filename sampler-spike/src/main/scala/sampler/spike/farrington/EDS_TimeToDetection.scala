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
  and calculates time to detect the outbreak for each simulation.
     
  Default is Scenario 14 in Noufaily et al., Statist. Med. 2013 (32) 1206-1222
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon (modified from EDS.scala by Oliver Tearne)
  Date:      03/03/2015
  Last edit: 04/03/2015
  
  ==========
  USER-DEFINED PARAMETERS:

  nSimulations
  nData
  endYear
  outbreakLength
  endBaseline
  endPreOutbreak
  endOutbreak
  
  csvName
  scriptName
  pdfName
  resultsDir
  
  =========
  FUNCTIONS:
  
  
  
  =========  
  OUTPUTS:
    
  detectTimes
  timesHist
  successRate
  
  
  */

object EDS_TimeToDetection  extends App{
  
  //=======================
  // User-defined parameters
  
  // Number of sets of data to simulate
  val nSimulations = 10
  
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
  val csvName = "timeToDetection.csv" // CSV file to store simulated data from Scala
  val scriptName = "plotTimeToDetection.r" // R script to import the CSV and plot the data
  val pdfName = "timeToDetection.pdf" // PDF containing the plots
  
  // Choose directory to place simulated data
  val resultsDir = Paths.get("results", "farrington")
  
  //=======================
  // Simulate outbreak data and calculate time to detect outbreak
  
  // For a single simulation: 
  ///*
  
  val magnitude = 10
  val data = GenerateData.run(
      nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
  import data._
  
  val detected = EDS_TS.run(data, endBaseline)
  val results = detected.results
  val flags = detected.flags
    
  val outbreakDetected = EDS_TS.detected(detected, start, end)
  
  val falsePositives = EDS_TS.falsePositives(detected, start, end)
  val FPR = EDS_TS.falsePositiveRate(detected, start, end)
  
  val times = EDS_TS.timeToDetection(detected, start, end)
  val TTD = if (times.size == 0) "none" else times(0)
  
  val hits = EDS_TS.hits(detected, start, end)
  
  
  //=======================
  // Print relevant information to console:
  
  println("Total no. of months = " + nData)
  
  println("Baseline period starts at " + 1 + " = " + year(0) + "-" + month(0))
  println("Pre-outbreak period starts at " + endBaseline + " = " + year(endBaseline) + "-" + month(endBaseline))
  println("Outbreak period starts at " + endPreOutbreak + " = " + year(endPreOutbreak) + "-" + month(endPreOutbreak))
  println("Post-outbreak period starts at " + endOutbreak + " = " + year(endOutbreak) + "-" + month(endOutbreak))
  
  println("Outbreak begins at month " + start + " = " + year(start-1) + "-" + month(start-1))
  println("Outbreak ends at month " + end + " = " + year(end-1) + "-" + month(end-1))
  
  println("Outbreak shape: " + hist)
  
  println("Outbreak detected: " + outbreakDetected)
  println("Hits = " + hits(0) + " of " + hits(1))
  println("False positives: " + falsePositives)
  println("False positive rate = " + FPR)
  println("Time to detection = " + TTD)
  
  

  //=======================
  // Visualisation
  
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  val timeSeriesJSON = 
    ("source" -> "Simulated data" ) ~
    ("month" -> results.map(_.date.yearMonth.toString)) ~
    ("monthId" -> results.map(_.date.idx)) ~
    ("expected" -> results.map(_.expected)) ~
    ("threshold" -> results.map(_.threshold)) ~
    ("actual" -> results.map(_.actual))
  //println(timeSeriesJSON)
  
  // Create html plot
  FreeMarkerHelper.writeFile(
    Map("jsonData" -> pretty(render(timeSeriesJSON))),
    "plot.ftl",
    resultsDir.resolve("output.html") 
  )
  
  //*/
  
  // For multiple simulations:
  /*

  RServeHelper.ensureRunning()
  val detectTimes =
    (0 until nSimulations).par.map{i =>
      val data = GenerateData.run(nData, endYear, outbreakLength, endPreOutbreak, endOutbreak)
      val detected = EDS.run(data, endBaseline)
      val res = timeToDetection.times(detected, data.start, data.hist)
      if (res.times.size == 0) -1 else res.times(0)
  }  
  RServeHelper.shutdown
  
  val timesHist = 
      detectTimes.groupBy(w => w).mapValues(_.size).toList.sorted
  println("Time to detection = " + timesHist)
  
  val successRate = (nSimulations - timesHist(1)._2).toDouble / nSimulations * 100
  println("Success rate = " + successRate + "%")
  
  // Write times to CSV file
  val writer = Files.newBufferedWriter(resultsDir.resolve(csvName), Charset.defaultCharset())
  writer.write("time, count")
  writer.newLine
  for (i <- 0 until timesHist.size) {
    writer.write(s"${timesHist(i)._1.toString}, ${timesHist(i)._2.toString}")
    writer.newLine
  }
  writer.close
  
  // Write R script which imports and plots data in a pdf
  val rScript = 
    s"""
      
    data = read.csv("$csvName")
      
    times = data[["time"]]
    count = data[["count"]]
    
    pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
    
    barplot(count,
          names.arg=as.character(times),
          main = "Time to detection",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
    
    dev.off()
    """
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))
  
  */
  
}