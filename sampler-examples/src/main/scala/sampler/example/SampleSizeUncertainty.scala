package sampler.example

import scala.collection.GenSeq
import org.apache.commons.math3.distribution.NormalDistribution
import sampler.Implicits.RichIndexedSeq
import sampler.data.Distribution
import sampler.data.ParallelSampler
import sampler.math.Random
import sampler.math.Statistics.maxDistance
import sampler.data.ConvergenceProtocol
import sampler.data.MaxMetric

/* 
 *  Given an imperfect test characterised by empirical data, how many samples should be taken to
 *  achieve a certain sensitivity of the approach?
 * 
 *  Empirical data on test performance is transformed into probability of detection when testing
 *  multiple infected replicates.  Sample size is increased until the desired sensitivity is
 *  achieved.
 */
object SampleSizeUncertainty extends App{
	implicit val random = Random
	
	def calcSampleSize(testPerformanceDistribution: Distribution[Double], requiredSensitivity: Double) = {
		// Transform the samples into a probability of detection given a sample size
		def testingApproachDistribution(numSamples: Int): Distribution[Boolean] = {
			testPerformanceDistribution.map{se => 
				random.nextDouble() < 1 - math.pow((1 - se), numSamples) 
			}
		}
		
		// Determine the smallest sample size which provides the desired sensitivity
		def loop(currentSampleSize: Int = 1): Int = {
			val model = testingApproachDistribution(currentSampleSize)
			
			val se = {
				val chunkSize = 1e4.toInt
				
				val dist = ParallelSampler.apply(model)(new ConvergenceProtocol[Boolean](chunkSize, 1e-5) with MaxMetric).toEmpiricalTable
				dist.probabilityTable(true)
			}
			
			println(f" - Sample Size $currentSampleSize => Sensitivity $se%.2f")
			
			if(se > requiredSensitivity) currentSampleSize
			else loop(currentSampleSize + 1)
		}	
		
		loop()
	}
	
	val requiredSensitivity = 0.95
		
	/* Generate some fake performance data.  In general this may be generated externally
	 * and loaded, for example using the [[sampler.io.ChainReader]].
	 */
	val normal = new NormalDistribution(0.7,0.9)
	val distributionWithVariance = Distribution(normal.sample)
			.filter(x => x > 0 && x < 1)
			.until(_.size == 1e5) 
			.sample
			.toEmpiricalSeq
			.toDistribution
	
	val distributionWithoutVariance = Distribution.continually(0.7)
			
	println("Calculating sample size when test has mean sensitivity 0.7, with variance 0.3")
	val result1 = calcSampleSize(distributionWithVariance, requiredSensitivity)
	println(s"$result1 samples are required to achieve Se = $requiredSensitivity\n")
	
	println("Calculating sample size when test has mean sensitivity 0.7, no variance")
	val result2 = calcSampleSize(distributionWithoutVariance, requiredSensitivity)
	println(s"$result2 samples are required to achieve Se = $requiredSensitivity")
}