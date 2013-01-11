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
import sampler.data.Empirical
import sampler.math.Probability
import sampler.data.Samplable

object NCP_Sensitivity extends App with EmpiricalMetricComponent{
  
  val home = Paths.get("", "examples", "ncpSampleSize", "data", "coda")
  
//  println(home.toAbsolutePath())
  
  val chains = ChainReader(home.toString())

//  NCP cage
  
  val requiredParameters = List(
	"PPosNCPFaecesCage[1]",
	"PPosNCPFaecesCage[2]",
	"PPosNCPFaecesCage[3]",
	"PPosNCPFaecesCage[4]",
	"PPosNCPFaecesCage[5]",
	"PPosNCPFaecesCage[6]"
  )

//  NCP non cage
  
//  val requiredParameters = List(
//	"PPosNCPFaecesNonCage[1]",
//	"PPosNCPFaecesNonCage[2]",
//	"PPosNCPFaecesNonCage[3]",
//	"PPosNCPFaecesNonCage[4]",
//	"PPosNCPFaecesNonCage[5]",
//	"PPosNCPFaecesNonCage[6]"
//  )
  
  val distMap = requiredParameters map (
      name => name -> (chains.get(name).get).toIndexedSeq) toMap

  implicit val r = new Random()
  
  val stats = new Statistics
  val requiredConf = 0.95
  
  val resultsMap = distMap map {case(k,v) => (k, sampleSizeCalc(v))}
  
  resultsMap.foreach(println(_))
  
  def sampleSizeCalc(model: IndexedSeq[Double]) = {
    
    def calcConf(size: Int) : Int = {
      val detectionSeq = model map (a => Probability(probDetection(a, size)))
      
      val samplable = Samplable.bernouliTrial(detectionSeq.toEmpiricalSeq)
    
      val builder = new ParallelSampleBuilder(2000)
	
      val results = builder(samplable)(terminationCondition _)
      
      if(conf(results) > requiredConf) {
        size
      } else {
        calcConf(size+1)  
      }
    }
    
    def conf(seq: ParSeq[Boolean]) = {
      seq.toEmpiricalTable.probabilities(true).value
    }
    
    calcConf(1)
  }

  def probDetection(p: Double, n: Int) = {
	  1-math.pow((1-p), n.toDouble)
  }

  def terminationCondition(soFar: GenSeq[Boolean]) = {
	  val distance = metric.max(soFar.toEmpiricalTable, soFar.take(soFar.size - 2000).toEmpiricalTable)

	  (distance < 0.0001) || (soFar.size > 1e8)
  }
}