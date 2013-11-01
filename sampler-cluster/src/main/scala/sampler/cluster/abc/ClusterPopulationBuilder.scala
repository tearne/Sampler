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

package sampler.cluster.abc

import sampler.abc.ABCModel
import sampler.abc.ABCParameters
import sampler.abc.EncapsulatedPopulation
import sampler.abc.builder.PopulationBuilder
import sampler.cluster.actor.PortFallbackSystemFactory
import sampler.math.Random
import sampler.cluster.abc.master.Dispatcher

/*
 * Takes the request to get a new generation, unwrap then EncapsulatedPopulation,
 * passes it to the dispatcher, and performs the nasty casting back on the results
 */
class ClusterPopulationBuilder(dispatcher: Dispatcher) extends PopulationBuilder{
	def run[M <: ABCModel](
			ePop: EncapsulatedPopulation[M], 
			abcParams: ABCParameters,
			tolerance: Double, 
			random: Random
	): Option[EncapsulatedPopulation[M]] = {
		//Every node gets the same job, and when we have  
		//enough particles the jobs get aborted
		val job = ABCJob(
			ePop.population, 
			tolerance,
			abcParams
		)
		
		//TODO Is there a way to eliminate this cast? 
		//It's needed since the ABCActorJob[T] type T can't
		//carry the model as required for model.Population
		val resultPopulation = dispatcher.apply(job).asInstanceOf[ePop.model.Population]
		Some(EncapsulatedPopulation(ePop.model)(resultPopulation))
	}
}
object ClusterPopulationBuilder{
	def startAndGet() = {
		val system = PortFallbackSystemFactory("ABCSystem")
		new ClusterPopulationBuilder(new Dispatcher(system))
	}
}