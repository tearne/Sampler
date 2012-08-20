package sampler.bats

import scala.collection.mutable.HashMap
import sampler.data.Empirical
import sampler.data.Distance

object TestSampler {

	def main(args: Array[String]) {
		val popSize = 100;
		val proportionInfected = 0.1
		val numToSample = 5

		val population = new Population(popSize, proportionInfected)

		var resultsSeq1 : IndexedSeq[Double] = IndexedSeq()
		var resultsSeq2 : IndexedSeq[Double] = IndexedSeq()
		
		
		for(i <- 0 until 2000) {
			resultsSeq1 = resultsSeq1 :+ population.sampleWithoutReplacement(numToSample).toDouble
		}
		
		resultsSeq2 = resultsSeq2 ++ resultsSeq1
		
		for(i <- 0 until 2000) {
			resultsSeq2 = resultsSeq2 :+ population.sampleWithoutReplacement(numToSample).toDouble
		}

		var eo1 = new Empirical(resultsSeq1)
		var eo2 = new Empirical(resultsSeq2)
		
		println("Initial diff: " + Distance.mean(eo1, eo2))
		
		var updates = 0;
		
		while(Distance.mean(eo1, eo2) > 0.0001) {
			val temp = eo2
			for(i <- 0 until 2000) {
				eo2 = eo2 + population.sampleWithoutReplacement(numToSample).toDouble
			}
			updates += 1
			eo1 = temp
		}
		
		println(eo2.size)
		println(eo2.counts)
		println(eo2.relFreq)
		
		println("Final diff: " + Distance.mean(eo1, eo2))
		println("Took " + updates + " cyles of iterations")
		
//		val withCountMap : HashMap[Int, Int] = HashMap()
//		val withoutCountMap : HashMap[Int, Int] = HashMap()
//
//		for(i <- 0 to numToSample) {
//			withCountMap += i -> 0
//			withoutCountMap += i -> 0
//		}
//
//		for(i <- 0 until 100000) {
//			val numDetectedWithReplace = population.sampleWithReplacement(numToSample)
//
//			withCountMap += numDetectedWithReplace -> (withCountMap(numDetectedWithReplace) + 1)
//		}
//
//		for(i <- 0 until 100000) {
//			val numDetectedWithoutReplace = population.sampleWithoutReplacement(numToSample)
//
//			withoutCountMap += numDetectedWithoutReplace -> (withoutCountMap(numDetectedWithoutReplace) + 1)
//		}
//
//		println("Sampling with replacement:")
//
//		withCountMap.foreach{(entry) => 
//			println(entry._1 + " -> " + entry._2)
//		}
//
//		println("\nSampling without replacement")
//
//		withoutCountMap.foreach{(entry) => 
//			println(entry._1 + " -> " + entry._2)
//		}
	}
}