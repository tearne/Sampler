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

package sampler.abc.builder.local

import sampler.abc.ABCModel
import sampler.run.ParallelRunner
import sampler.run.Abortable
import sampler.run.RunnerComponent
import sampler.abc.ABCParameters
import sampler.math.Random
import sampler.io.Logging
import sampler.abc.MaxRetryException
import scala.util.Failure
import sampler.abc.MaxRetryException
import sampler.abc.MaxRetryException
import scala.util.Success
import sampler.abc.MaxRetryException
import sampler.abc.EncapsulatedPopulation
import sampler.abc.builder.PopulationBuilder
import sampler.abc.builder.ParticleBuilderComponent

trait LocalPopulationBuilder 
		extends PopulationBuilder
		with ParticleBuilderComponent
		with JobBuilderComponent
		with Logging	// Added because of Logging in JobComponent
		with RunnerComponent {
	
	def run[M <: ABCModel](
		ePop: EncapsulatedPopulation[M], 
		abcParams: ABCParameters,
		tolerance: Double, 
		random: Random
	): Option[EncapsulatedPopulation[M]] = {
		//TODO
		
	  //Build the jobs (JobBuilderComponent, uses ParticleBuilderComponent)
	  //TODO think about the Random
	  val jobs = jobBuilder.makeJobs(ePop)(abcParams, tolerance, random)
		
	  //Run the jobs (Runner)
	  val result = runner.apply(jobs)
		
	  //Process success/failures and return results (here)
		
	  val successes = result.collect{
			case Success(s) => s
	  }.flatten
		
	  if(successes.size == abcParams.numParticles)
			Some(EncapsulatedPopulation(ePop.model)(successes))
	  else{
		val badException = result.collectFirst{
		  case Failure(e) if !e.isInstanceOf[MaxRetryException] => e
		}
		badException.foreach{throw _}
			
//		log.warn("Expected {}, got {}", abcParams.numParticles, successes.size)
		None
	  }
	}
}

object LocalPopulationBuilder extends LocalPopulationBuilder {
  val particleBuilder = new ParticleBuilder{}
  val jobBuilder = new JobBuilder{}
  val runner = ParallelRunner()
}

//=============================================
//TODO Stuff below here will be refactored into the components stuff


/*
class LocalPopulationBuilder(runner: Runner) extends PopulationBuilder with Logging{
	def run[M <: ABCModel](
			ePop: EncapsulatedPopulation[M], 
			abcParams: ABCParameters,
			tolerance: Double, 
			random: Random
		): Option[EncapsulatedPopulation[M]] = {
		
*		//Run the building in parallel on local machine, by chunking
*		val jobSizes = (1 to abcParams.numParticles)
*				.grouped(abcParams.particleChunking)
*				.map(_.size).toList
*		log.info(s"Tolerance = $tolerance, Local job sizes = $jobSizes")
*		val jobs = jobSizes.map{quantity => Abortable{aborter =>
*			PopulationBuilder(ePop.model)(ePop.population, quantity, tolerance, aborter, abcParams, random)
*		}}
		
		val rawResults = runner.apply(jobs)
		
		val successes = rawResults.collect{
			case Success(s) => s
		}.flatten
		
		if(successes.size == abcParams.numParticles)
			Some(EncapsulatedPopulation(ePop.model)(successes))
		else{
			val badException = rawResults.collectFirst{
				case Failure(e) if !e.isInstanceOf[MaxRetryException] => e
			}
			badException.foreach{throw _}
			
			log.warn("Expected {}, got {}", abcParams.numParticles, successes.size)
			None
		}
	}
}
object LocalPopulationBuilder{
	def apply() = new LocalPopulationBuilder(ParallelRunner())
}

*/