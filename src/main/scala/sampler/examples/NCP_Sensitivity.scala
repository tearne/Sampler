package sampler.examples

import java.nio.file.Paths
import eu9302.winbugs.ChainReader
import sampler.data.Empirical._
import sampler.math.Random
import sampler.data.ParallelSampleBuilder
import scala.collection.GenSeq
import sampler.data.EmpiricalMetricComponent
import sampler.data.EmpiricalTable
import scala.collection.parallel.ParSeq
import sampler.math.Statistics

object NCP_Sensitivity extends App with EmpiricalMetricComponent{
  
//  DATA READ IN
  
  val home = Paths.get("", "examples", "ncpSampleSize", "data", "coda")
  
//  println(home.toAbsolutePath())
  
  val chains = ChainReader(home.toString())
  
  val requiredParameters = List(
	"PPosNCPFaecesCage[1]"
//	"PPosNCPFaecesCage[2]",
//	"PPosNCPFaecesCage[3]",
//	"PPosNCPFaecesCage[4]",
//	"PPosNCPFaecesCage[5]",
//	"PPosNCPFaecesCage[6]"
  )
  
  val distMap = requiredParameters map (
      name => name -> (chains.get(name).get).toIndexedSeq.toEmpiricalTable) toMap

//  END DATA READ IN
      
//  ANALYSIS
      
//  val firstPrev = distMap(distMap.keySet.toList(0))
//  val secondPrev = distMap(distMap.keySet.toList(1))
//  val thirdPrev = distMap(distMap.keySet.toList(2))
//  val fourthPrev = distMap(distMap.keySet.toList(3))
//  val fifthPrev = distMap(distMap.keySet.toList(4))
//  val sizthPrev = distMap(distMap.keySet.toList(5))
      
      val firstPrev = IndexedSeq(1,2,3,4,5).toEmpiricalTable
//      val secondPrev = IndexedSeq(6,7,8,9,10).toEmpiricalTable
  
  implicit val r = new Random()
  
  println(firstPrev.sample)
//  println(secondPrev.sample)
  
  val stats = new Statistics
  val requiredConf = 0.95
  
//  val resultMap = distMap map {case(k, v) => (k, sampleSizeCalc(v, List()))}
//  distMap foreach{case(k,v) => println(k + " " + sampleSizeCalc(v, List())._1)}
  
//  val sampleSizeMap = resultMap map {case(k, v) => (k, v._1)}
//  
//  println(sampleSizeMap)
  
  def sampleSizeCalc(model: EmpiricalTable[Double], accum: List[Double]) : (Double, List[Double]) = {
    if(accum.size > 0 && accum.last >= requiredConf) (accum.size, accum)
    else {
      val size = accum.size + 1
      
      sampleSizeCalc(model, accum.:+(stats.mean(sampleBuilder(model, 2000, size).toEmpiricalTable)))
    }
  }
  
  def sampleBuilder(model: EmpiricalTable[Double], chunkSize: Int, sampleSize: Int) = {
    def takeMore(previous: ParSeq[Double]): ParSeq[Double] = {
      if(terminationCondition(previous)) previous
      else takeMore(
        previous ++ (1 to chunkSize).par.map(i => probDetection(model.sample, sampleSize))
      )
    }
    
    def terminationCondition(soFar: GenSeq[Double]) = {
      val distance = metric.max(
   		soFar.take(soFar.size - 2000).toEmpiricalTable,
    	soFar.toEmpiricalTable
      )
    			
      (distance < 0.001) || (soFar.size > 1e8)
    }
    
    def probDetection(p: Double, n: Int) = {
    	1-math.pow((1-p), n.toDouble)
    }
    
    val kickstart = (1 to chunkSize).par.map(i => probDetection(model.sample, sampleSize))
    takeMore(kickstart)
  }
  
}