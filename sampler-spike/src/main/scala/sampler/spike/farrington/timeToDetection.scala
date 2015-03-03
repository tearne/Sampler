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

/*
  =========
  NOTES:

  Calculates time taken for Early Detection System to detect an outbreak.
  Uses the Farrington algorithm, which calculates the maximum number
  of outbreak cases that should be expected each month (threshold) and
  returns an alert if the actual count exceeds this value.
  
  Follows the method outlined in Farrington et al., 1996
  
  Uses default parameters to simulate baseline and outbreak data
  (Scenario 14 in Noufaily et al., Statist. Med. 2013 (32) 1206-1222)
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon (modified from EDS.scala)
  Date:      02/03/2015
  Last edit: 03/03/2015
  
  ==========
  USER-DEFINED PARAMETERS:

  data            Outbreak data as type GenerationResult  
  endBaseline     Month in which baseline period ends
  exclusions      Set of dates to exclude from Farrington algorithm
  stop            Boolean to determine if code should stop at first detection.
                  stop == false runs Farrington for all of the data
                  and returns all detection times.  
  
  =========
  FUNCTIONS:
  
  runAll
  runUntilDetection
  
  =========  
  OUTPUTS:
    
  results
  flags
  
  */

case class DetectionResult(
    results: IndexedSeq[Result],
    flags: IndexedSeq[Int]
)

case class FlagResult (
    times: IndexedSeq[Int],
    falsePositives: IndexedSeq[Int]
)

object timeToDetection extends App{
  
  def run(
      data: GenerationResult,
      endBaseline: Int,
      exclusions: Set[YearMonth] = Set.empty,
      stop: Boolean = false
    ): DetectionResult = {
    
    val year = data.year
    val month = data.month 
    val countData = data.counts
    val tOutbreak = data.start
    
    val nData = countData.size
        
    // Create TreeMap with form (YearMonth, Count)
    val dataOutbreak_all = TreeMap{
    	(0 until nData).map{ i => YearMonth.of(year(i), month(i)) -> countData(i) }: _*
    }
    
    // Exclude set of months if necessary
    val dataOutbreak = dataOutbreak_all.--(exclusions)
    
    //=======================
    // Calculate time to detection
    
    RServeHelper.ensureRunning()
    val rCon = new RConnection
    try {    	
    	val indexedData = EDS.indexAndExclude(dataOutbreak, exclusions)
  		val maxDrop = nData - (endBaseline + 1)
      if (stop == false) runAll(maxDrop, indexedData, rCon)
      else runUntilDetection(maxDrop, indexedData, rCon)
      }    
    finally {
  	  rCon.close
  	  RServeHelper.shutdown
    }
            
  }
  
  def times(
      data: DetectionResult,
      tOutbreak: Int,
      histData: List[(Int, Int)]
    ): FlagResult = {
    
    val tEnd = tOutbreak + histData.last._1    
    
    val outbreakFlags = data.flags.intersect(tOutbreak to tEnd) 
    val falsePositives = data.flags.diff(tOutbreak to tEnd) 
    
    val times = outbreakFlags.map(i => i - tOutbreak)
    
    FlagResult(times, falsePositives)
    
  }
  
  def runAll(
      maxDrop: Int,
      indexedData: SortedMap[Date, Int],
      rCon: RConnection
    ): DetectionResult = {
    def loop(i: Int, acc: IndexedSeq[Result], flags: IndexedSeq[Int]): DetectionResult = {
        val series = EDS.extractWindow(indexedData.dropRight(i))
        val x = Farrington.run(series, rCon)
        val detected = if (x.isAlert) flags :+ indexedData.size - i else flags
        if (i == 0) DetectionResult(acc :+ x, detected)
        else loop(i-1, acc :+ x, detected)
      }
    loop(maxDrop, IndexedSeq(), IndexedSeq())
  }
  
  def runUntilDetection(
      maxDrop: Int,
      indexedData: SortedMap[Date, Int],
      rCon: RConnection
    ): DetectionResult = {
    def loop(i: Int, acc: IndexedSeq[Result]): DetectionResult = {
        val series = EDS.extractWindow(indexedData.dropRight(i))
        val x = Farrington.run(series, rCon)
        val index = IndexedSeq(indexedData.size - i)
        if (x.isAlert) DetectionResult(acc :+ x, index)
        else loop(i-1, acc :+ x)
      }
    loop(maxDrop, IndexedSeq())
  }
  
}