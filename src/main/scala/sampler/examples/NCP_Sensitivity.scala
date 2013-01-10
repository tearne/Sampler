package sampler.examples

import java.nio.file.Paths
import eu9302.winbugs.ChainReader
import sampler.data.Empirical._
import sampler.math.Random
import sampler.data.ParallelSampleBuilder
import scala.collection.GenSeq
import sampler.data.EmpiricalMetricComponent

object NCP_Sensitivity extends App with EmpiricalMetricComponent{
  
//  DATA READ IN
  
  val home = Paths.get("", "examples", "ncpSampleSize", "data", "coda")
  
//  println(home.toAbsolutePath())
  
  val chains = ChainReader(home.toString())
  
  val requiredParameters = List(
	"PPosEUFaecesCage[1]",
	"PPosEUFaecesCage[2]",
	"PPosEUFaecesCage[3]",
	"PPosEUFaecesCage[4]",
	"PPosEUFaecesCage[5]",
	"PPosEUFaecesCage[6]"
  )
  
  val distMap = requiredParameters map (
      name => name -> (chains.get(name).get).toIndexedSeq.toEmpiricalTable) toMap

//  END DATA READ IN
      
//  ANALYSIS
      
  val firstPrev = distMap(distMap.keySet.toList(0))
  
  implicit val r = new Random()
  
  def terminationCondition(soFar: GenSeq[Double]) = {
			val distance = metric.max(
			    soFar.take(soFar.size - 2000).toEmpiricalTable,
			    soFar.toEmpiricalTable
			)
			
			(distance < 0.001) || (soFar.size > 1e8)
		}
  
  val sampleSize = 10;
  
  val selectedSe = firstPrev.sample

  val detected = probDetection(selectedSe, sampleSize)
  
  println("With sensitivity " + selectedSe + " and sample size " + sampleSize + " the probability of detection of disease is " + detected)
  
  def probDetection(p: Double, n: Int) = {
    1-math.pow((1-p), n.toDouble)
  }
}