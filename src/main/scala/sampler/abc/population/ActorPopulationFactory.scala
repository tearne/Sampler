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

package sampler.abc.population

import scala.util.Try
import scala.language.existentials
import akka.actor.ActorSystem
import sampler.abc.ABCModel
import sampler.data.Empirical
import sampler.run.ActorJob
import sampler.run.ActorJobRunner
import sampler.run.WrappedAborter
import sampler.run.akka.FailFastRunner
import sampler.run.akka.worker.Executor
import sampler.run.akka.NodeApplication

class ActorFactory(runner: ActorJobRunner) extends PopulationFactory{
	def run(model: ABCModel)(pop: Empirical[model.Parameters], jobSizes: Seq[Int], tolerance: Double): Seq[Try[model.Population]] = {
		val jobs = jobSizes.map{quantity =>
			Job(pop, quantity, tolerance)
		}
		
		//TODO Is there a way to eliminate this cast? 
		//It's needed since the ABCActorJob[T] type T can't
		//carry the model as required for model.Population
		runner(jobs).asInstanceOf[Seq[Try[model.Population]]]
	}
}
object ActorPopulationFactory{
	
	def tasker(system: ActorSystem) = new ActorFactory(new FailFastRunner(system))
}

case class Job(samplablepopulation: Empirical[_], quantity: Int, tolerance: Double) extends ActorJob[ABCModel#Population]