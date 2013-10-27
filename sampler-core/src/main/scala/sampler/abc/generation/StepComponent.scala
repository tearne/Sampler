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

package sampler.abc.generation

import sampler.io.Logging
import sampler.abc.ABCParameters
import sampler.math.Random
import sampler.abc.ABCModel
import sampler.abc.population.EncapsulatedPopulation
import sampler.abc.population.PopulationBuilder
import scala.util.Try
import scala.util.Failure
import sampler.abc.MaxRetryException
import scala.util.Success
import sampler.abc.Particle

protected[abc] trait StepComponent { 
	self: Logging =>
		
	val step: Step

	trait Step {
		def apply[M <: ABCModel](
				ePop: EncapsulatedPopulation[M], 
				pBuilder: PopulationBuilder, 
				abcParams: ABCParameters, 
				tolerance: Double, 
				random: Random
		): Option[EncapsulatedPopulation[M]] = {
			val jobSizes = (1 to abcParams.numParticles)
				.grouped(abcParams.particleChunking)
				.map(_.size).toList
			log.info(s"Tolerance = $tolerance, Job sizes = $jobSizes")
			
			val results: Seq[Try[EncapsulatedPopulation[M]]] = pBuilder.run(ePop, jobSizes, tolerance, abcParams, random)
			
			val failures = results.collect{
				case Failure(e: MaxRetryException) => Right(e)
				case Failure(e) => Left(e)
			}
			
			failures.collectFirst{case Left(e) => throw e}			
			
			val exceptions = results.collect{case Failure(e) => e}
			if(failures.size > 0) {
				log.warn("{} exception(s) thrown building population.  First: {}", failures.size, failures(0))
				None
			}
		    else Some{
		    	import ePop.model._
		    	val a = results.map{_.map{_.population}}
		    	val b = a.collect{
		    		//TODO MS: Don't like the cast
		    		case Success(p: Seq[_]) => p.asInstanceOf[Population]
		    	}
		    	val c = b.flatten
		    	EncapsulatedPopulation(ePop.model)(c)
		    }
		}
	}
}