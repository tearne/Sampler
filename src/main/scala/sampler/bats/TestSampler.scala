package sampler.bats

import scala.collection.mutable.HashMap
import sampler.data.Empirical
import sampler.data.Distance

object TestSampler {

	def main(args: Array[String]) {
		val popSize = 100;
		val proportionInfected = 0.1
		val numToSample = 10

		val population = new Population(popSize, proportionInfected)

		val precision = 0.1
		val confidence = 0.95
		
		val sampleSizeCalculator = new SampleSizeCalculator()
		
		val minSampleSize = sampleSizeCalculator.bestSampleSize(
				population, proportionInfected, precision, confidence)
				
		println("The minimum sample size required to detect a prevalence of " + proportionInfected + 
				" (precision " + precision + ") with confidence of " + (confidence*100).toInt + "% is " + minSampleSize)
	}
}