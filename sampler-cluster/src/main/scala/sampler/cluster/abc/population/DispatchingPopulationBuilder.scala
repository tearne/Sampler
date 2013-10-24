/*
 * Copyright (c) 2012-13 Crown Copyright 
 *                       Animal Health and Veterinary Laboratories Agency
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

package sampler.cluster.abc.population

import scala.util.Try
import scala.language.existentials
import akka.actor.ActorSystem
import sampler.abc.ABCModel
import sampler.data.Empirical
import sampler.abc.ABCParameters
import sampler.math.Random
import sampler.cluster.actor.client.dispatch.Dispatcher
import sampler.cluster.actor.FailFastDispatcher
import sampler.cluster.actor.client.dispatch.Job
import sampler.abc.population.PopulationBuilder
import sampler.abc.population.EncapsulatedPopulation

class DispatchingPopulationBuilder(dispatcher: Dispatcher) extends PopulationBuilder{
	def run[M <: ABCModel](
			ePop: EncapsulatedPopulation[M], 
			jobSizes: Seq[Int], 
			tolerance: Double, 
			meta: ABCParameters,
			random: Random
	): Seq[Try[EncapsulatedPopulation[M]]] = {
		val jobs = jobSizes.map{quantity =>
			ABCJob(ePop.population, quantity, tolerance, meta)
		}
		
		//TODO Is there a way to eliminate this cast? 
		//It's needed since the ABCActorJob[T] type T can't
		//carry the model as required for model.Population
		val results = dispatcher(jobs).asInstanceOf[Seq[Try[ePop.model.Population]]].map{t => t.map{pop => EncapsulatedPopulation(ePop.model)(pop)}}
		assert(results.size == jobs.size, "Results set not of expected size.  Check logs for Worker exceptions")
		results
	}
}
object DispatchingPopulationBuilder{
	def apply(system: ActorSystem) = new DispatchingPopulationBuilder(new FailFastDispatcher(system))
}

case class ABCJob(population: Seq[_], quantity: Int, tolerance: Double, meta: ABCParameters) extends Job[ABCModel#Population]