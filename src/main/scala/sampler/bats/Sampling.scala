package sampler.bats

import sampler.data.Empirical

class SampleSizeCalculator() {

	def bestSampleSize(population: Population, precision: Double, confidence: Double): Int = {
		
//		Pick a sample size to start with (5 as an initial guess for testing)

		val numToSample = 10
		
//		Sample population
		
		var resultsSeq1 : IndexedSeq[Double] = IndexedSeq()
		
		for(i <- 0 until 2000) {
			val samplePrevalence = (population.sampleWithoutReplacement(numToSample).toDouble) / numToSample
			resultsSeq1 = resultsSeq1 :+ samplePrevalence
		}
		
		var eo1 = new Empirical(resultsSeq1)
		
		println(eo1.relFreq)
		
//		Convert results to prevalence
	
//		Work out converged distribution

//		Decide if acceptable result
		
		0
	
	}
	
} 
