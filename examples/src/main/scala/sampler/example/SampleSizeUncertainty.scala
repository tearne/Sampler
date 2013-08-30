package sampler.example

import sampler.data.Samplable
import sampler.math.Random
import scala.collection.GenSeq
import sampler.math.StatisticsComponent
import sampler.data.Empirical._
import sampler.data.ParallelSampleBuilder
import sampler.math.Probability

/** Given an imperfect test characterised by empirical data, how many samples should be taken to
 *  achieve a certain sensitivity of the approach?
 * 
 *  Empirical data on test performance is transformed into probability of detection when testing
 *  multiple infected replicates.  Sample size is increased until the desired sensitivity is
 *  achieved.
 */
object SampleSizeUncertainty extends App{
	implicit val random = Random
	
	def calcSampleSize(testPerformanceDistribution: Samplable[Double], requiredSensitivity: Probability) = {
	
	/** Transform the samples into a probability of detection given a sample size */
	def testingApproachSamplable(numSamples: Int): Samplable[Boolean] = {
		testPerformanceDistribution.map{se => 
			random.nextDouble() < 1 - math.pow((1 - se), numSamples) 
		}
	}
	
	/** Determine the smallest sample size which provides the desired sensitivity */
	def loop(currentSampleSize: Int = 1): Int = {
		val model = testingApproachSamplable(currentSampleSize)
		
		val se = {
			val chunkSize = 1e4.toInt
			def stopCondition(soFar: GenSeq[Boolean]) = {
				//TODO make simple metrics like this available off the shelf?
				val distance = StatisticsComponent.maxDistance(
			    	    soFar.toEmpiricalTable, 
			    	    soFar.take(soFar.size - (chunkSize-1)).toEmpiricalTable	// TODO confirm -1 change
		    	)
		    	
		    	(distance < 1e-5) || (soFar.size > 1e8)
			}
			
			val dist = new ParallelSampleBuilder(chunkSize)(model)(stopCondition).toEmpiricalTable
			dist.probabilityTable(true)
		}
		
		println(f" - Sample Size $currentSampleSize => Sensitivity ${se.value}%.2f")
		
		if(se > requiredSensitivity) currentSampleSize
		else loop(currentSampleSize + 1)
	}	
	
	loop()
	}
	
	val requiredSensitivity = Probability(0.95)
		
	/** Generate some fake performance data.  In general this may be generated externally
	 *  and loaded, for example using the [[sampler.io.ChainReader]].
	 */
	val distributionWithVariance = 
		Samplable.normal(0.7, 0.3)
			.filter(x => x > 0 && x < 1)
			.until(_.size == 1e5) 
			.sample						//TODO Samplable.flatten to replace some of this?
			.toEmpiricalSeq
			.toSamplable
	
	val distributionWithoutVariance = Samplable.continually(0.7)
			
	println("Calculating sample size when test has mean sensitivity 0.7, with variance 0.3")
	val result1 = calcSampleSize(distributionWithVariance, requiredSensitivity)
	println(s"$result1 samples are required to achieve Se = $requiredSensitivity\n")
	
	println("Calculating sample size when test has mean sensitivity 0.7, no variance")
	val result2 = calcSampleSize(distributionWithoutVariance, requiredSensitivity)
	println(s"$result2 samples are required to achieve Se = $requiredSensitivity")
}