package sampler.examples.bats

import sampler.math.Random
import sampler.data.Samplable
import sampler.data.Distance
import sampler.data.Empirical
import sampler.math.Probability
import sampler.util.Implicits

object AnotherOnePopulation extends App with Implicits{
	/*
	 * In a population of a given size, and sampling with replacement,
	 * how many samples should be taken to be % confident of observing
	 * the correct population prevalence (within specific precision)?
	 */
	
	val start = System.currentTimeMillis
	
	//Domain parameters
	val popSize = 100;
	val truePrev = 0.1
	val precision = 0.09
	val confidence = Probability(0.95)
	
	//Meta-parameters
	val chunkSize = 2000
	val convergenceCriterion = 0.0001
	
	implicit val random = new Random
	val numInfected = (popSize*truePrev).round.toInt
	val population = (1 to popSize).map(_ < numInfected)
		
	def empiricalObjective(numSampled: Int) = {
		Samplable.withoutReplacement(population, numSampled)	// Start with base model
		.map(_.count(identity) / numSampled.toDouble)			// Transform to model of sample prevalance
		.parallelBuildEmpirical(chunkSize){samples =>			// Sample the model until convergence
			val distance = Distance.mean(Empirical(samples.seq.take(samples.size - chunkSize)), Empirical(samples.seq))
			(distance < convergenceCriterion) || (samples.size > 1e8)
		}
		.map(samplePrev => math.abs(samplePrev - truePrev) < precision)	// Transform samples to in/out of tolerance
	}
	
	val result = {
		(1 to popSize).view
			.map{n => 
				val eo = empiricalObjective(n)
				val confidence = eo.relativeFreq(true).getOrElse(Probability.zero)
				println("Sample size = %d, empirical size = %d, confidence = %s".format(n, eo.size, confidence.toString))
				(n, confidence)
			}
			.find(_._2 > confidence)
			.get._1
	}
	
	println("Estimated minimum sample size required to detect a prevalence of " + truePrev + 
				" (precision " + precision + ") with confidence of " + confidence + "% is " + result)
	
	println("took " + (System.currentTimeMillis - start) + "ms.")
}