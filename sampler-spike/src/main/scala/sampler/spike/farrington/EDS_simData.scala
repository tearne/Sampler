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
	
	// Define end of each period
	//Baseline -> Pre-outbreak -> Outbreak -> Post-outbreak
	val endBaseline = 146
	val endPreOutbreak = 182
	val endOutbreak = 282
			
  // Identifiers for results files
  val csvName = "timeToDetection.csv" // CSV file to store simulated data from Scala
  val scriptName = "plotTimeToDetection.r" // R script to import the CSV and plot the data
  val pdfName = "timeToDetection.pdf" // PDF containing the plots
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "farrington")
  
  //=======================
  // Simulate outbreak data
    
  val data = GenerateData.run(nData, endYear, outbreakLength, endPreOutbreak, endOutbreak)
  
  val year = data.year
  val month = data.month 
  val countData = data.counts
  val histData = data.hist
  val tOutbreak = data.start
  
  val outbreakDuration = histData.size
  val tEnd = tOutbreak + outbreakDuration - 1
  
  val detected = EDS.run(data, endBaseline)
  
  val results = detected.results
  
  //=======================
  // Print relevant information to console:
  
  println("Total no. of months = " + nData)
  
  println("Baseline period starts at " + 1 + " = " + year(0) + "-" + month(0))
  println("Pre-outbreak period starts at " + endBaseline + " = " + year(endBaseline) + "-" + month(endBaseline))
  println("Outbreak period starts at " + endPreOutbreak + " = " + year(endPreOutbreak) + "-" + month(endPreOutbreak))
  println("Post-outbreak period starts at " + endOutbreak + " = " + year(endOutbreak) + "-" + month(endOutbreak))
  
  println("Outbreak begins at month " + tOutbreak + " = " + year(tOutbreak-1) + "-" + month(tOutbreak-1))
  println("Outbreak occurs during months " + tOutbreak + "-" + tEnd)
    
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
  
  //=======================
  // Function definitions
  
  def indexAndExclude(
      obsByDate: SortedMap[YearMonth, Int], 
      exclusions: Set[YearMonth] = Set.empty
      ): SortedMap[Date, Int] = {
    assert(!obsByDate.exists{case (ym, _) => exclusions.contains(ym)})
    
    val removedExclusions = obsByDate.filterKeys{ym => !exclusions.contains(ym)}
    val firstDate = removedExclusions.firstKey
        
        implicit val dateOrdering = Ordering.by{d: Date => d.idx}
    
    removedExclusions.map{case (ym, count) => Date(ym, MONTHS.between(firstDate, ym)) -> count}
  }
  
  def extractWindow(timeSeries: SortedMap[Date, Int]): SortedMap[Date, Int] = {
    val lastObsDate = timeSeries.lastKey
        val window = List(-1, 0, 1).map(v => (v + 12) % 12)
        val windowLowerBound = lastObsDate.yearMonth.minus(12, YEARS).minus(1, MONTHS)
        
        def keep(date: Date) = {
      val monthRemainder = MONTHS.between(date.yearMonth, lastObsDate.yearMonth) % 12
          val inWindow = window.exists(_ == monthRemainder)
          
          val isAfterStartDate = windowLowerBound.compareTo(date.yearMonth) <= 0 
          val isBeforeEndDate = MONTHS.between(date.yearMonth, lastObsDate.yearMonth) > 2
          val isBaseline = inWindow && isAfterStartDate && isBeforeEndDate
          
          isBaseline || date == lastObsDate
    }
    val t = timeSeries.filterKeys(keep)
        t
  }
  
}