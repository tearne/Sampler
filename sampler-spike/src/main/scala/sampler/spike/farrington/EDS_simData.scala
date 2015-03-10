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
  Last edit: 26/02/2015
  
  ==========
  USER-DEFINED PARAMETERS:

  nData           No. of months for which to simulate data
  
  outbreakLength  Length of outbreak ("short" or "long")
               
  endBaseline     Month in which baseline period ends
  endPreOutbreak  Month in which pre-outbreak period ends
  endOutbreak     Month in which outbreak period ends
  
  =========
  FUNCTIONS:
  
  indexAndExclude
  extractWindow  
  
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
  
  val magnitude = 2
  
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
  import data._
  
  RServeHelper.ensureRunning()
  val detected = EDS_TS.run(data, endBaseline)
  RServeHelper.shutdown
  
  val results = detected.results
  
  //=======================
  // Print relevant information to console:
  
  println("Total no. of months = " + nData)
  
  println("Baseline period starts at " + 1 + " = " + year(0) + "-" + month(0))
  println("Pre-outbreak period starts at " + endBaseline + " = " + year(endBaseline) + "-" + month(endBaseline))
  println("Outbreak period starts at " + endPreOutbreak + " = " + year(endPreOutbreak) + "-" + month(endPreOutbreak))
  println("Post-outbreak period starts at " + endOutbreak + " = " + year(endOutbreak) + "-" + month(endOutbreak))
  
  println("Outbreak begins at month " + start + " = " + year(start-1) + "-" + month(start-1))
  println("Outbreak occurs during months " + start + "-" + end)
  println("Outbreak counts " + hist)
  
  //=======================
  // Visualisation
  
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  // Write times to CSV file
  val writer = Files.newBufferedWriter(resultsDir.resolve(csvName), Charset.defaultCharset())
  writer.write("month, baseline, outbreak, start, end")
  writer.newLine
  writer.write(s"${1.toString}, ${data.baseline(0).toString}, ${data.counts(0).toString}, ${start.toString}, ${end.toString}")
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
          main = "Time to detection",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
    
    dev.off()
    """
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))
  
  
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
  
  
  
}