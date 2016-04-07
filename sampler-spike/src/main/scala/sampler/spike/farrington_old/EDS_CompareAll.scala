package sampler.spike.farrington_old

/*
  =========
  NOTES:
  Simulate outbreak data and run an Early Detection System
  to compare the outputs of various versions of the Farrington algorithm,
  which calculates the maximum number of outbreak cases
  that should be expected each month and signals an alert if this threshold
  is breached.
  
  Follows the method outlined in Farrington et al., 1996
  
  Uses default parameters to simulate baseline and outbreak data
  (Scenario 14 in Noufaily et al., Statist. Med. 2013 (32) 1206-1222)
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon
  Date:      17/03/2015
  Last edit: 17/03/2015
  
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
import sampler.spike.farrington_old.Farrington.Mode
import sampler.spike.farrington_old.Farrington.APHA
import sampler.spike.farrington_old.Farrington.Stl
import sampler.spike.farrington_old.Farrington.FarNew
import sampler.r.process.ScriptRunner
import farrington.core.script.CreateRScript


object EDS_CompareAll extends App{
  
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
  
  // Magnitude of outbreak
  val magnitude = 5
  
  // No of years back to use in Farrington to calculate threshold
  val nYearsBack = 12
  
  // Identifiers for results files
  val csvName = "compareEDSoutput.csv"
  val scriptName = "compareEDSoutput.r" // R script to import the CSV and plot the data
  val pdfName = "compareEDSoutput.pdf" // PDF containing the plots
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "compareFarrington")
  
  // Input data
  val cl = getClass.getClassLoader
  val input = cl.getResource("farrington/input.txt").toURI()
  
  // Exclusions
  val exclude = 2001
  
  //=======================
  // Input data, and index and exclude
  
  val data = TreeMap{
    Source.fromFile(input)
      .getLines()
      .filter(_.trim.size > 0)
      .zipWithIndex
      .map{case (line, idx) =>
        val toks = line.split("\t").map(_.trim.toInt)
        val year = toks(0)
        val month = toks(1)
        val incidentCount = toks(3)
        YearMonth.of(year, month) -> incidentCount
      }
      .toSeq: _*
  }
  
  // Create set of dates to exclude
  val exclude2001 = (1 to 12).map{m => YearMonth.of(2001, m)}.to[Set]
  
  val indexedData = EDS.indexAndExclude(data, exclude2001)
  
  //=======================
  // Run EDS

  // APHA model
  RServeHelper.ensureRunning()
  val rCon = new RConnection
  val EDS_APHA = try{
    val mode = Farrington.APHA
    EDS.runAll(indexedData, rCon, mode, nYearsBack)
  } finally {
    rCon.close
    RServeHelper.shutdown
  }
  
  // Farrington new model
  RServeHelper.ensureRunning()
  val rCon2 = new RConnection
  val EDS_FarNew = try{
    val mode = Farrington.FarNew
    EDS.runAll(indexedData, rCon2, mode, nYearsBack)
  } finally {
    rCon2.close
    RServeHelper.shutdown
  }
  
  // Stl model
  RServeHelper.ensureRunning()
  val rCon3 = new RConnection
  val EDS_Stl = try{
    val mode = Farrington.Stl
    EDS.runAll(indexedData, rCon3, mode, nYearsBack)
  } finally {
    rCon3.close
    RServeHelper.shutdown
  }
  
  //=======================
  // Visualisation: EDS
  
  Files.createDirectories(resultsDir)
  
  val nAPHA = EDS_APHA.results.threshold.length
  val nFarNew = EDS_FarNew.results.threshold.length
  
  val diff = nAPHA - nFarNew
  val APHA_thresh = EDS_APHA.results.threshold.drop(diff)
  val FarNew_thresh = EDS_FarNew.results.threshold
  val Stl_thresh = EDS_Stl.results.threshold.drop(diff)
  
  // Write times to CSV file
  val writerEDS = Files.newBufferedWriter(resultsDir.resolve(csvName), Charset.defaultCharset())
  writerEDS.write("month, count, APHA, FarNew, Stl")
  writerEDS.newLine
  for (i <- 0 until nFarNew) {
    writerEDS.write(s"${EDS_FarNew.results.date(i).idx.toString}, ${EDS_FarNew.results.actual(i).toString}, ${APHA_thresh(i).toString}, ${FarNew_thresh(i).toString}, ${Stl_thresh(i).toString}")
    writerEDS.newLine
  }
  writerEDS.close
  
  val rScript = CreateRScript.plotComparison(csvName, pdfName)
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript, resultsDir.resolve(scriptName)) 

}