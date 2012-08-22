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

		val sampleSizeCalculator = new SampleSizeCalculator()
		
		sampleSizeCalculator.bestSampleSize(population, 0.0, 0.0)
	}
}