package farrington.core.measures

import java.nio.file.Files
import java.nio.charset.Charset
import scala.reflect.io.Path

case class AverageMeasures(
    POD: Double,
    POCD: Double,
    FPR: Double,
    FPRCon: Double,
    PPV: Double,
    PPVCon: Double,
    TTD: Double,
    TTCD: Double,
    POTD: Double
)

case object AverageMeasures{
  
  def calculate(measures: IndexedSeq[Measures], nSimulations: Int) = {
    
    // Probability of detection
    val pod = measures.map(_.POD).count(i => i==true).toDouble / nSimulations
    // Probability of consecutive detection
    val pocd = measures.map(_.POCD).count(i => i==true).toDouble / nSimulations
    // False positive rate
    val fpr = measures.map(_.FPR).sum.toDouble / nSimulations
    // False positive rate (consecutive)
    val fprCon = measures.map(_.FPRCon).sum.toDouble / nSimulations
    // Positive predictive value
    val ppv = measures.map(_.PPV).sum.toDouble / nSimulations
    // Positive predictive value (consecutive)
    val ppvCon = measures.map(_.PPVCon).sum.toDouble / nSimulations
    // Time to detection
    val times = measures.map(_.TTD).filter(i => i.length > 0).map(_.head)
    val ttd = times.sum.toDouble / times.length
    // Time to detection (consecutive)
    val timesCon = measures.map(_.TTCD).filter(i => i.length > 0).map(_.head)
    val ttcd = timesCon.sum.toDouble / timesCon.length
    // Proportion of outbreak times detected
    val potd = measures.map(_.POTD).sum.toDouble / nSimulations
    
    AverageMeasures(pod, pocd, fpr, fprCon, ppv, ppvCon, ttd, ttcd, potd)
    
  }
  
  def print(avgMeasures: AverageMeasures): Unit = {
    import avgMeasures._
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
  
  def toString(avgMeasures: AverageMeasures): String = {
    s"${avgMeasures.POD},${avgMeasures.POCD},${avgMeasures.FPR},${avgMeasures.FPRCon},${avgMeasures.PPV},${avgMeasures.PPVCon},${avgMeasures.TTD},${avgMeasures.TTCD},${avgMeasures.POTD}"
  }
  
}