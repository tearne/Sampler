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

package sampler.abc.population

import scala.util.Try
import scala.language.existentials
import akka.actor.ActorSystem
import sampler.abc.ABCModel
import sampler.data.Empirical
import sampler.run.actor.dispatch.FailFastDispatcher
import sampler.run.actor.dispatch.Job
import sampler.run.actor.dispatch.Dispatcher
import sampler.abc.ABCParameters

class ActorPopulationDispatcher(dispatcher: Dispatcher) extends PopulationBuilder{
	def run(model: ABCModel)(pop: Empirical[model.Parameters], jobSizes: Seq[Int], tolerance: Double, meta: ABCParameters): Seq[Try[model.Population]] = {
		val jobs = jobSizes.map{quantity =>
			ABCJob(pop, quantity, tolerance, meta)
		}
		
		//TODO Is there a way to eliminate this cast? 
		//It's needed since the ABCActorJob[T] type T can't
		//carry the model as required for model.Population
		dispatcher(jobs).asInstanceOf[Seq[Try[model.Population]]]
	}
}
object ActorPopulationDispatcher{
	def apply(system: ActorSystem) = new ActorPopulationDispatcher(new FailFastDispatcher(system))
}

case class ABCJob(samplablepopulation: Empirical[_], quantity: Int, tolerance: Double, meta: ABCParameters) extends Job[ABCModel#Population]