package sampler.bats

import sampler.data.Distance
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._
import sampler.data.FrequencyTable

class SampleSizeCalculator() {

	def bestSampleSize(population: Population, prevalence: Double, precision: Double, confidence: Double): Int = {
		
		var resultsMap = new HashMap[Int, Double]
		var minimumSampleSize = 0
		
		breakable { for(numToSample <- 0 to population.popSize) {
		
//			Sample population
		
			var resultsSeq1 : IndexedSeq[Double] = IndexedSeq()
			var resultsSeq2 : IndexedSeq[Double] = IndexedSeq()
		
			for(i <- 0 until 2000) {
				resultsSeq1 = resultsSeq1 :+ (population.sampleWithoutReplacement(numToSample).toDouble) / numToSample
				resultsSeq2 = resultsSeq2 :+ (population.sampleWithoutReplacement(numToSample).toDouble) / numToSample
			}
		
			var eo1 = new FrequencyTable(resultsSeq1)
			var eo2 = new FrequencyTable(resultsSeq2)
		
			var numUpdates = 0
		
//			Work out when sample distribution has converged
		
			while(Distance.mean(eo1, eo2) > 0.0001) {
				var tempEO = eo2
				for(i <- 0 until 2000) {
							eo2 = eo2.+((population.sampleWithoutReplacement(numToSample).toDouble) / numToSample)
				}
			
				eo1 = tempEO
				numUpdates = numUpdates + 1
			}
		
			val lowerBound = prevalence - precision
			val upperBound = prevalence + precision
		
			var totalResultInRange = 0.0
		
			eo1.probabilityMap.map{_ match {
				case (x, y) if (x >= lowerBound && x <= upperBound) => totalResultInRange = totalResultInRange + y.value
				case _ =>
			}}
		
			resultsMap = resultsMap.+=((numToSample, totalResultInRange))
		
			println(numToSample + " => " + totalResultInRange + "\t" + numUpdates*2000 + " samples")

			if(totalResultInRange > confidence){
//				var prevConf = resultsMap.get(numToSample-1).getOrElse(0.0)
//				if(prevConf > confidence){
//					minimumSampleSize = numToSample-1
					minimumSampleSize = numToSample
					break				
//				}
//				
			}
		
		}}
		
		minimumSampleSize
	}
}

class PrevalenceCalculator(){
	
	def resultsDistribution(expectedPrevalence: Double, precision: Double, 
			popSize: Int, sampleSize: Int) = {
		
		for(prev <- 0 to 100) {
		
			val prevalence = prev.toDouble/100
			
			val population = new Population(popSize, prevalence)
			
			var resultsSeq1 : IndexedSeq[Double] = IndexedSeq()
			var resultsSeq2 : IndexedSeq[Double] = IndexedSeq()
	
			for(i <- 0 until 2000) {
				resultsSeq1 = resultsSeq1 :+ (population.sampleWithoutReplacement(sampleSize).toDouble) / sampleSize
				resultsSeq2 = resultsSeq2 :+ (population.sampleWithoutReplacement(sampleSize).toDouble) / sampleSize
			}
	
			var eo1 = new FrequencyTable(resultsSeq1)
			var eo2 = new FrequencyTable(resultsSeq2)
	
			var numUpdates = 0
	
			//			Work out when sample distribution has converged
	
			while(Distance.mean(eo1, eo2) > 0.0001) {
				var tempEO = eo2
						for(i <- 0 until 2000) {
							eo2 = eo2.+((population.sampleWithoutReplacement(sampleSize).toDouble) / sampleSize)
						}
	
				eo1 = tempEO
				numUpdates = numUpdates + 1
			}
			
			val lowerBound = expectedPrevalence - precision
			val upperBound = expectedPrevalence + precision
	
			var totalResultInRange = 0.0
		
			eo1.probabilityMap.map{_ match {
				case (x, y) if (x >= lowerBound && x <= upperBound) => totalResultInRange = totalResultInRange + y.value
				case _ =>
			}}
			
			println(prevalence + " => " + totalResultInRange)
		}
	}
}
