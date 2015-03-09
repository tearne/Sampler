package sampler.spike.farrington

// Author: Oliver Tearne
// EDS as of 09/02/2015
// For reference only - do not edit

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

object EDS_original extends App{
  val resultsDir = Paths.get("results", "farrington")
  Files.createDirectories(resultsDir)
  
  val cl = getClass.getClassLoader
  val input = cl.getResource("farrington/input.txt").toURI()
  
  val exclude2001 = (1 to 12).map{m => YearMonth.of(2001, m)}.to[Set]
  
  val countData = TreeMap{
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
  
  RServeHelper.ensureRunning()
  val rCon = new RConnection
  val results = try{
    val indexedData = indexAndExclude(countData, exclude2001)
  
    (0 to 144).map{i => 
//      Farrington.run(
//          extractWindow(indexedData.dropRight(i)), 
//          rCon, Farrington.FarNew
//      )
      
//      Farrington.run(
//          indexedData.dropRight(i), 
//          rCon, Farrington.APHA
//      )
      Farrington.run(
          indexedData.dropRight(i), 
          rCon, Farrington.Stl
      )
    }
  } finally {
    rCon.close
    RServeHelper.shutdown
  }

  val timeSeriesJSON = 
    ("source" -> input.toString()) ~
    ("month" -> results.map(_.date.yearMonth.toString)) ~
    ("monthId" -> results.map(_.date.idx)) ~
    ("expected" -> results.map(_.expected)) ~
    ("threshold" -> results.map(_.threshold)) ~
    ("actual" -> results.map(_.actual))
    
  FreeMarkerHelper.writeFile(
    Map("jsonData" -> pretty(render(timeSeriesJSON))),
    "plot.ftl",
    resultsDir.resolve("output.html") 
  ) 
  
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