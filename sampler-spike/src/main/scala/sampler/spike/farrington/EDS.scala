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

  Early Detection System for detecting outbreaks in a set of counts.
  Uses the Farrington algorithm, which calculates the maximum number
  of outbreak cases that should be expected each month (the threshold) and
  returns an alert if the actual count exceeds this value.
  
  Follows the method outlined in
   - Farrington et al., 1996
   - Noufaily et al., Statist. Med. 2013 (32) 1206-1222
    
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon (modified from EDS_original.scala)
  Date:      05/03/2015
  Last edit: 05/03/2015
  
  ==========
  USER-DEFINED PARAMETERS:

  data            Outbreak data as type GenerationResult  
  endBaseline     Month in which baseline period ends
  stop            Boolean to determine if code should stop at first detection.
                  stop == false runs Farrington for all of the data
                  and returns all detection times.  
  exclusions      Set of dates to exclude from Farrington algorithm
  
  =========
  FUNCTIONS:
  
  indexAndExclude
  extractWindow
  runAll
  runUntilDetection
  runUntilConsecutive
  
  =========  
  OUTPUTS:
      
  
  */

case class FarringtonResult(
    results: IndexedSeq[Result],
    flags: IndexedSeq[Int]
)

object EDS extends App{
	
  def run(
      data: GenerationResult,
      endBaseline: Int,
      stop: String = "false",
      exclusions: Set[YearMonth] = Set.empty
    ): FarringtonResult = {
    
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

    val rCon = new RConnection
    try {     
      val indexedData = EDS.indexAndExclude(dataOutbreak, exclusions)
      val maxDrop = nData - (endBaseline + 1)
      if (stop == "false") runAll(maxDrop, indexedData, rCon)
      else if (stop == "detect") runUntilDetection(maxDrop, indexedData, rCon)
      else runUntilConsecutive(maxDrop, indexedData, rCon)
      }    
    finally {
      rCon.close
    }
            
  }
  
  //=======================
  // FUNCTION DEFINITIONS 
	
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
  
  def runAll(
      maxDrop: Int,
      indexedData: SortedMap[Date, Int],
      rCon: RConnection
    ): FarringtonResult = {
    def loop(i: Int, acc: IndexedSeq[Result], flags: IndexedSeq[Int]): FarringtonResult = {
      val series = EDS.extractWindow(indexedData.dropRight(i))
        val x = Farrington.run(series, rCon)
        val detected = if (x.isAlert) flags :+ (indexedData.size - i) else flags
        if (i == 0) FarringtonResult(acc :+ x, detected)
        else loop(i-1, acc :+ x, detected)
      }
    loop(maxDrop, IndexedSeq(), IndexedSeq())
  }
  
  def runUntilDetection(
      maxDrop: Int,
      indexedData: SortedMap[Date, Int],
      rCon: RConnection
    ): FarringtonResult = {
    def loop(i: Int, acc: IndexedSeq[Result]): FarringtonResult = {
        val series = EDS.extractWindow(indexedData.dropRight(i))
        val x = Farrington.run(series, rCon)
        val index = IndexedSeq(indexedData.size - i)
        if (x.isAlert || i == 0) FarringtonResult(acc :+ x, index)
        else loop(i-1, acc :+ x)
      }
    loop(maxDrop, IndexedSeq())
  }
  
  def runUntilConsecutive(
      maxDrop: Int,
      indexedData: SortedMap[Date, Int],
      rCon: RConnection
    ): FarringtonResult = {
    def loop(i: Int, acc: IndexedSeq[Result]): FarringtonResult = {
        val series = EDS.extractWindow(indexedData.dropRight(i))
        val x = Farrington.run(series, rCon)
        if (i == 0) {
          FarringtonResult(acc :+ x, IndexedSeq(indexedData.size - i))
        }
        else { if (acc.size == 0) {
          loop(i-1, acc :+ x)
        }
        else { if (x.isAlert && acc.last.isAlert) {
          FarringtonResult(acc :+ x, IndexedSeq(indexedData.size - i))
        } else {
          loop(i-1, acc :+ x)
        } } }
      }
    loop(maxDrop, IndexedSeq())
  }
  
  // Returns list of times to detection of all alerts during outbreak
  def timeToDetection(
      data: FarringtonResult,
      tStart: Int,
      tEnd: Int
    ): IndexedSeq[Int] = {    
    val outbreakFlags = data.flags.intersect(tStart to tEnd)
    if (outbreakFlags.size == 0) IndexedSeq() 
    else outbreakFlags.map(i => i - tStart)    
  }
  
  // Proportion of alerts made during outbreak to total outbreak months
  def proportionDetected(data: FarringtonResult, tStart: Int, tEnd: Int) = {
	  val nDetected = data.flags.intersect(tStart to tEnd).length
	  nDetected.toDouble / (tEnd - tStart + 1)
  }
  
  // Number of months during outbreak that an alert was made
  def hits(data: FarringtonResult, tStart: Int, tEnd: Int) = {
    val nDetected = data.flags.intersect(tStart to tEnd).length
    List(nDetected.toDouble, (tEnd - tStart + 1))
  }
  
  // Returns list of months in which false positives occured
  def falsePositives(data: FarringtonResult, tStart: Int, tEnd: Int) =   
    data.flags.diff(tStart to tEnd)
  
  // Returns proportion of false positives
  def falsePositiveRate(data: FarringtonResult, tStart: Int, tEnd: Int) = {
    val noOutbreak = data.results.length - (tEnd - tStart + 1)
    EDS.falsePositives(data, tStart, tEnd).length.toDouble / noOutbreak
  }
  
  // Returns boolean depending on whether outbreak has been detected
  def detected(data: FarringtonResult, tStart: Int, tEnd: Int ): Boolean = {    
	  val times = EDS.timeToDetection(data, tStart, tEnd)    
			  if (times.size == 0) false else true    
  }
  
  /*
  // Returns boolean depending on whether outbreak has been detected
  def detectedConsecutive(data: FarringtonResult, tStart: Int, tEnd: Int ): Boolean = {    
    val times = EDS.timeToDetection(data, tStart, tEnd)    
      if (times.size == 0) false
      else {
        def loop(i: Int): Boolean = {
          if (times(i) == times(i-1) + 1) true
          else { if (i == times.length + 1) false
          else loop(i+1)
          }
        }
        loop(1)
      }
  }
  * 
  */
  
}