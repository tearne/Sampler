package sampler.bats

import scala.collection.mutable.HashMap

object TestSampler {

	def main(args: Array[String]) {
		val popSize = 100;
		val proportionInfected = 0.1
		val numToSample = 5

		val population = new Population(popSize, proportionInfected)

		val withCountMap : HashMap[Int, Int] = HashMap()
		val withoutCountMap : HashMap[Int, Int] = HashMap()

		for(i <- 0 to numToSample) {
			withCountMap += i -> 0
			withoutCountMap += i -> 0
		}

		for(i <- 0 until 100000) {
			val numDetectedWithReplace = population.sampleWithReplacement(numToSample)

			withCountMap += numDetectedWithReplace -> (withCountMap(numDetectedWithReplace) + 1)
		}

		for(i <- 0 until 100000) {
			val numDetectedWithoutReplace = population.sampleWithoutReplacement(numToSample)

			withoutCountMap += numDetectedWithoutReplace -> (withoutCountMap(numDetectedWithoutReplace) + 1)
		}

		println("Sampling with replacement:")

		withCountMap.foreach{(entry) => 
			println(entry._1 + " -> " + entry._2)
		}

		println("\nSampling without replacement")

		withoutCountMap.foreach{(entry) => 
			println(entry._1 + " -> " + entry._2)
		}
	}
}