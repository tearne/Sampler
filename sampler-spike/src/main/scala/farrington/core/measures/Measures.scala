package farrington.core.measures

import farrington.core.result.FarringtonResult
import java.nio.file.Path
import java.nio.charset.Charset
import java.nio.file.Files

case class Measures(
    POD: Boolean,
    POCD: Boolean,
    FPR: Double,
    FPRCon: Double,
    PPV: Double,
    PPVCon: Double,
    TTD: IndexedSeq[Int],
    TTCD: IndexedSeq[Int],
    POTD: Double
)
case object Measures {
  
  // Returns list of times to detection of all alerts during outbreak
  def timeToDetection(data: FarringtonResult, tStart: Int, tEnd: Int): IndexedSeq[Int] = {    
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
  
  // Returns list of months in which false positives occurred
  def falsePositives(data: FarringtonResult, tStart: Int, tEnd: Int) =   
    data.flags.diff(tStart to tEnd)
  
  // Returns proportion of false positives
  def falsePositiveRate(data: FarringtonResult, tStart: Int, tEnd: Int) = {
    val nOutbreak = data.results.actual.length - (tEnd - tStart + 1)
    falsePositives(data, tStart, tEnd).length.toDouble / nOutbreak
  }
  
  def trueNegativeRate(data: FarringtonResult, tStart: Int, tEnd: Int) = {
    val n = data.results.actual.length - (tEnd - tStart + 1)
    val nFP = falsePositives(data, tStart, tEnd).length
    (n - nFP).toDouble/n
  }
  
  def positivePredictive(data: FarringtonResult, tStart: Int, tEnd: Int) = {
    val nTP = data.flags.intersect(tStart to tEnd).length
    val nFP = falsePositives(data, tStart, tEnd).length
    if (nTP + nFP == 0) 1 else nTP.toDouble / (nTP + nFP)
  }
  
  // Returns consecutive detections
  def flagsConsecutive(data: FarringtonResult, tStart: Int, tEnd: Int) = {
    if (data.flags.length <= 1) IndexedSeq()
    else {
      def loop(i: Int, acc: IndexedSeq[Int]): IndexedSeq[Int] = {
        if (i == data.flags.length) acc
        else { if (data.flags(i) - data.flags(i-1) == 1) loop(i+1, acc :+ data.flags(i))
        else loop(i+1, acc)
        }
      }
      loop(1, IndexedSeq())
    }
  }
  
  def fpConsecutive(data: FarringtonResult, tStart: Int, tEnd: Int) = {
    flagsConsecutive(data, tStart, tEnd).diff(tStart to tEnd)
  }
  
  def fprConsecutive(data: FarringtonResult, tStart: Int, tEnd: Int) = {
    val n = data.results.actual.length - (tEnd - tStart + 1)
    flagsConsecutive(data, tStart, tEnd).diff(tStart to tEnd).length.toDouble / n
  }
  
  def tnrConsecutive(data: FarringtonResult, tStart: Int, tEnd: Int) = {
    val n = data.results.actual.length - (tEnd - tStart + 1)
    val nFP = fpConsecutive(data, tStart, tEnd).length
    (n - nFP).toDouble/n
  }
  
  def ppvConsecutive(data: FarringtonResult, tStart: Int, tEnd: Int) = {
    val nTP = flagsConsecutive(data, tStart, tEnd).intersect(tStart to tEnd).length
    val nFP = fpConsecutive(data, tStart, tEnd).length
    if (nTP + nFP == 0) 1 else nTP.toDouble / (nTP + nFP)
  }
  
  // Returns list of times to detection of all alerts during outbreak
  def timeToConsecutiveDetection(
      data: FarringtonResult,
      tStart: Int,
      tEnd: Int
    ): IndexedSeq[Int] = {    
    val outbreakFlags = flagsConsecutive(data, tStart, tEnd).intersect(tStart to tEnd)
    if (outbreakFlags.size == 0) IndexedSeq() 
    else outbreakFlags.map(i => i - tStart)    
  }
  
  // Returns boolean depending on whether outbreak has been detected
  def detected(data: FarringtonResult, tStart: Int, tEnd: Int ): Boolean = {    
    val times = timeToDetection(data, tStart, tEnd)    
        if (times.length == 0) false else true    
  }  
  
  // Returns boolean depending on whether outbreak has been detected at consecutive times
  def detectedConsecutive(data: FarringtonResult, tStart: Int, tEnd: Int ): Boolean = {    
    val flags = flagsConsecutive(data, tStart, tEnd).intersect(tStart to tEnd)
      if (flags.length == 0) false else true
  }
  
  def allMeasures(data: FarringtonResult, tStart: Int, tEnd: Int ) = {
    val alert = detected(data, tStart, tEnd)
    val consecutive = detectedConsecutive(data, tStart, tEnd)
    val fpr = falsePositiveRate(data, tStart, tEnd)
    val fprCon = fprConsecutive(data, tStart, tEnd)
    val ppv = positivePredictive(data, tStart, tEnd)
    val ppvCon = ppvConsecutive(data, tStart, tEnd)
    val ttd = timeToDetection(data, tStart, tEnd)
    val ttcd = timeToConsecutiveDetection(data, tStart, tEnd)
    val potd = proportionDetected(data, tStart, tEnd)
    Measures(alert, consecutive, fpr, fprCon, ppv, ppvCon, ttd, ttcd, potd)    
  }
  
  def print(measures: Measures): Unit = {
    import measures._
    println("Probability of any detection = " + POD)  
    println("Probability of consecutive detection = " + POCD)    
    println("False positive rate = " + FPR)    
    println("False positive rate (consecutive) = " + FPRCon)    
    println("Positive prediction value = " + PPV)    
    println("Positive prediction value (consecutive) = " + PPVCon)
    println("Time to detection = " + TTD)    
    println("Time to detection (consecutive) = " + TTCD) 
    println("Proportion of outbreak times detected = " + POTD)
  }
  
  // Write times to detection to CSV file for APHA
  def writeTTD(ttdHist: List[(Int, Int)], path: Path, csvName: String) = {
    val writer = Files.newBufferedWriter(path.resolve(csvName), Charset.defaultCharset())
    writer.write("time, count")
    writer.newLine
    for (i <- 0 until ttdHist.length) {
      writer.write(s"${ttdHist(i)._1.toString}, ${ttdHist(i)._2.toString}")
      writer.newLine
    }
    writer.close
  }
  
}