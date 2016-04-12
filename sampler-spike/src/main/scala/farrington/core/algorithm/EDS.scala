package farrington.core.algorithm

import scala.collection.SortedMap
import org.rosuda.REngine.Rserve.RConnection
import farrington.core.algorithm.Farrington.Mode
import farrington.core.result.FarringtonResult
import farrington.core.result.Result
import farrington.core.result.FarringtonResult2
import farrington.core.algorithm.Farrington.FarNew
import farrington.core.result.Date
import farrington.core.algorithm.Farrington.APHA
import java.time._
import java.time.Year
import java.time.ZoneId
import java.time.temporal.ChronoUnit._
import java.time.temporal.ChronoField
import java.time.Duration.of
import java.time.Duration
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalUnit
import farrington.core.result.ResultVector
import scala.collection.immutable.TreeMap
import farrington.core.simulate.OutbreakData
import sampler.r.rserve.RServeHelper
import sampler.r.rserve.RServeHelper

object EDS {
  
  // HELPER FUNCTIONS:
  
  // Adds indices to the year-month and removes exclusions
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
  
  
  // Extracts a window of one month either side of the current month for each year
  def extractWindow(timeSeries: SortedMap[Date, Int], mode: Mode = APHA, nYearsBack: Int = 12): SortedMap[Date, Int] = {
    val lastObsDate = timeSeries.lastKey
    val window =
      if (mode == APHA) List(-1, 0, 1).map(v => (v + 12) % 12)
      else (0 to 11).toList
    val windowLowerBound = 
      if (mode == APHA) lastObsDate.yearMonth.minus(nYearsBack, YEARS).minus(1, MONTHS)
      else lastObsDate.yearMonth.minus(nYearsBack, YEARS)
    def keep(date: Date) = {
      val monthRemainder = MONTHS.between(date.yearMonth, lastObsDate.yearMonth) % 12
      val inWindow = window.exists(_ == monthRemainder)
    
      val isAfterStartDate = windowLowerBound.compareTo(date.yearMonth) <= 0 
      val isBeforeEndDate = 
        if (mode == APHA) MONTHS.between(date.yearMonth, lastObsDate.yearMonth) > 2
        else MONTHS.between(date.yearMonth, lastObsDate.yearMonth) > -1
      val isBaseline = inWindow && isAfterStartDate && isBeforeEndDate
    
      isBaseline || date == lastObsDate
    }
    val t = timeSeries.filterKeys(keep)
    t
  }
  
  def vectoriseResult(output: FarringtonResult2): FarringtonResult = {
    import output._
    val n = results.length
    val dateVec = results.map(_.date)
    val actualVec = results.map(_.actual)
    val expectedVec = results.map(_.expected)
    val thresholdVec = results.map(_.threshold)
    val trendVec = results.map(_.trend)
    val exceedVec = results.map(_.exceed)
    val weightsVec = results(0).weights
    val resultsVec = ResultVector(dateVec, actualVec, expectedVec, thresholdVec, trendVec, exceedVec, weightsVec)
    FarringtonResult(resultsVec, flags)
  }
  
  // EDS ALGORITHMS:
  
  def run(
      data: OutbreakData,
//      rCon: RConnection,
      endBaseline: Int,
      mode: Mode = APHA,
      nYearsBack: Int = 12,
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
      val indexedData = indexAndExclude(dataOutbreak, exclusions)
      if (stop == "false") runAll(indexedData, rCon, mode, nYearsBack)
      else if (stop == "detect") runUntilDetection(indexedData, rCon, mode, nYearsBack)
      else runUntilConsecutive(indexedData, rCon, mode, nYearsBack)
    }
    finally {
      rCon.close
    }
    
//    val indexedData = indexAndExclude(dataOutbreak, exclusions)
//    if (stop == "false") runAll(indexedData, rCon, mode, nYearsBack)
//    else if (stop == "detect") runUntilDetection(indexedData, rCon, mode, nYearsBack)
//    else runUntilConsecutive(indexedData, rCon, mode, nYearsBack)
            
  }
  
  // Runs EDS without stopping if flag is found
  def runAll(
      indexedData: SortedMap[Date, Int],
      rCon: RConnection,
      mode: Mode = APHA,
      nYearsBack: Int = 5
    ): FarringtonResult = {
    if (mode == FarNew) {
      val x = Farrington.runFarringtonNew(indexedData, rCon, mode, nYearsBack)
      val flags = x.date
        .zipWithIndex.filter(i => x.isAlert(i._2) == true)
        .map(i => i._1.idx.toInt)
      FarringtonResult(x, flags)      
    } else {
      val maxDrop = indexedData.size - nYearsBack*12
      def loop(i: Int, acc: IndexedSeq[Result], flags: IndexedSeq[Int]): FarringtonResult2 = {
        val series = extractWindow(indexedData.dropRight(i), mode, nYearsBack)
        val x = Farrington.runFarrington(series, rCon, mode)
        val detected = if (x.isAlert) flags :+ x.date.idx.toInt else flags
        if (i == 0) FarringtonResult2(acc :+ x, detected)
        else loop(i-1, acc :+ x, detected)
        }
      val result = loop(maxDrop, IndexedSeq(), IndexedSeq())
      vectoriseResult(result)
    }
  }
  
  // Runs EDS until flag is detected
  def runUntilDetection(
      indexedData: SortedMap[Date, Int],
      rCon: RConnection,
      mode: Mode = APHA,
      nYearsBack: Int = 5
    ): FarringtonResult = {
    if (mode == FarNew) {
      val x = Farrington.runFarringtonNew(indexedData, rCon, mode, nYearsBack)
      val flags = x.date
        .zipWithIndex.filter(i => x.isAlert(i._2) == true)
        .map(i => i._1.idx.toInt)
      FarringtonResult(x, flags)
    } else {
      val maxDrop = indexedData.size - nYearsBack*12
      def loop(i: Int, acc: IndexedSeq[Result]): FarringtonResult2 = {
          val series = 
            if (mode == FarNew) indexedData.dropRight(i)
            else EDS.extractWindow(indexedData.dropRight(i), mode)
          val x = Farrington.runFarrington(series, rCon, mode)
          val index = IndexedSeq(x.date.idx.toInt)
          if (x.isAlert) FarringtonResult2(acc :+ x, index)
          else { if (i == 0) FarringtonResult2(acc :+ x, IndexedSeq())
          else loop(i-1, acc :+ x)
          }
        }
      val result = loop(maxDrop, IndexedSeq())
      vectoriseResult(result)
    }
  }
  
  
  // Runs EDS until two consecutive flags are found
  def runUntilConsecutive(
      indexedData: SortedMap[Date, Int],
      rCon: RConnection,
      mode: Mode = APHA,
      nYearsBack: Int = 5
    ): FarringtonResult = {
    if (mode == FarNew) {
      val x = Farrington.runFarringtonNew(indexedData, rCon, mode, nYearsBack)
      val flags = x.date
        .zipWithIndex.filter(i => x.isAlert(i._2) == true)
        .map(i => i._1.idx.toInt)
      FarringtonResult(x, flags)
    } else {
      val maxDrop = indexedData.size - nYearsBack*12
      def loop(i: Int, acc: IndexedSeq[Result], last: Boolean): FarringtonResult2 = {
          val series = 
            if (mode == FarNew) indexedData.dropRight(i)
            else EDS.extractWindow(indexedData.dropRight(i), mode)
          val x = Farrington.runFarrington(series, rCon, mode)
          if (i == 0) {
            FarringtonResult2(acc :+ x, IndexedSeq(x.date.idx.toInt))
          }
          else { if (acc.size == 0) {
            loop(i-1, acc :+ x, x.isAlert)
          }
          else { if (x.isAlert && last) {
            FarringtonResult2(acc :+ x, IndexedSeq(x.date.idx.toInt))
          } else {
            loop(i-1, acc :+ x, x.isAlert)
          } } }
        }
      val result = loop(maxDrop, IndexedSeq(), false)
      vectoriseResult(result)
    }
  }

}