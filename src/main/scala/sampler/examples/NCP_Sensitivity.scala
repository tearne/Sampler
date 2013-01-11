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
  
//  DATA READ IN
  
  val home = Paths.get("", "examples", "ncpSampleSize", "data", "coda")
  
//  println(home.toAbsolutePath())
  
  val chains = ChainReader(home.toString())
  
  val requiredParameters = List(
	"PPosNCPFaecesCage[1]",
	"PPosNCPFaecesCage[2]",
	"PPosNCPFaecesCage[3]",
	"PPosNCPFaecesCage[4]",
	"PPosNCPFaecesCage[5]",
	"PPosNCPFaecesCage[6]"
  )
  
  val distMap = requiredParameters map (
      name => name -> (chains.get(name).get).toIndexedSeq) toMap

//  END DATA READ IN
      
//  ANALYSIS
      
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
      seq.count(_ == true).toDouble/seq.size.toDouble
    }
    
    calcConf(1)
  }

  def probDetection(p: Double, n: Int) = {
	  1-math.pow((1-p), n.toDouble)
  }

  def terminationCondition(soFar: GenSeq[Boolean]) = {
	  def ratio(seq: GenSeq[Boolean]) = {
		  soFar.count(_ == true).toDouble/soFar.count(_ == false).toDouble
	  }

	  val distance = math.abs(ratio(soFar) - ratio(soFar.take(soFar.size - 2000)))

	  (distance < 0.0001) || (soFar.size > 1e8)
  }
}