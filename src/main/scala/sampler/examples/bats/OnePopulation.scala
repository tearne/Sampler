/*
 * Copyright (c) 2012 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.examples.bats

import sampler.math.Random
import sampler.data.Samplable
import sampler.data.Distance
import sampler.data.Empirical
import sampler.math.Probability
import sampler.data.Distribution
import sampler.data.FrequencyTableBuilder
import sampler.data.FrequencyTable

object AnotherOnePopulation extends App{
	/*
	 * In a population of a given size, and sampling with replacement,
	 * how many samples should be taken to be % confident of observing
	 * the correct population prevalence (within specific precision)?
	 */
	
	val start = System.currentTimeMillis
	
	//Domain parameters
	val popSize = 100
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
		val model = 
			Distribution.withoutReplacement(population, numSampled)	// Start with base model
			.map(_.count(identity) / numSampled.toDouble)			// Transform to model of sample prevalance
		
		// Sample the model until convergence
		FrequencyTableBuilder.parallel(model, chunkSize){samples =>			
			val distance = Distance.mean(FrequencyTable(samples.seq.take(samples.size - chunkSize)), FrequencyTable(samples.seq))
			(distance < convergenceCriterion) || (samples.size > 1e8)
		}
		.map(samplePrev => math.abs(samplePrev - truePrev) < precision)		// Transform samples to in/out of tolerance
	}
	
	val result = {
		(1 to popSize).view
			.map{n => 
				val eo = empiricalObjective(n)
				val confidence = eo.probabilityMap.get(true).getOrElse(Probability.zero)
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