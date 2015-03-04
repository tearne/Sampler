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
    
  
  
  
  */

object EDS_HiddenOutbreak extends App{
  
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
    
  // For multiple simulations:
  ///*

  RServeHelper.ensureRunning()
  val detectTimes =
    (0 until nSimulations).par.map{i =>
      
      println(i)
      
      val data = GenerateData.run(nData, endYear, outbreakLength, endPreOutbreak, endOutbreak)
      val splitData = GenerateData.splitOutbreak(data)
      
      val dataFull = timeToDetection.run(data, endBaseline)
      val full = timeToDetection.times(dataFull, data.start, data.hist)
      val tFull = if (full.times.size == 0) -1 else full.times(0)
      
      val dataSplit1 = timeToDetection.run(splitData.data1, endBaseline)
      val split1 = timeToDetection.times(dataSplit1, data.start, data.hist)
      val tSplit1 = if (split1.times.size == 0) -1 else split1.times(0)
      
      val dataSplit2 = timeToDetection.run(splitData.data2, endBaseline)
      val split2 = timeToDetection.times(dataSplit2, data.start, data.hist)
      val tSplit2 = if (split2.times.size == 0) -1 else split2.times(0)
      
      IndexedSeq(tFull, tSplit1, tSplit2)
      
  }  
  RServeHelper.shutdown
  
  val histFull =
    detectTimes.map(i => i(0)).groupBy(w => w).mapValues(_.size).toList.sorted
  val histSplit1 =
    detectTimes.map(i => i(1)).groupBy(w => w).mapValues(_.size).toList.sorted
  val histSplit2 =
    detectTimes.map(i => i(2)).groupBy(w => w).mapValues(_.size).toList.sorted
  
  println("Time to detection for full data = " + histFull)
  println("Time to detection for split 1 data = " + histSplit1)
  println("Time to detection for split 2 data = " + histSplit2)
    
  def successRate(nTotal: Int, hist: List[(Int, Int)]) = {
    val nMisses =
      if (hist.count(i => i._1 == -1) == 0) 100
      else hist(0)._2
	  ((nTotal - nMisses).toDouble / nTotal) * 100
  }
  
  println("Success rate for full data = " + 
      successRate(nSimulations, histFull) + "%")
  println("Success rate for split 1 data = " + 
      successRate(nSimulations, histSplit1) + "%")
  println("Success rate for split 2 data = " + 
      successRate(nSimulations, histSplit2) + "%")


}