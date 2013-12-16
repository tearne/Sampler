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

import scala.concurrent.Await
import scala.concurrent.duration._
import akka.util.Timeout
import akka.actor.Props
import sampler.cluster.abc.actor.worker.AbortableModelRunner
import sampler.cluster.abc.actor.root.RootActor
import sampler.cluster.actor.PortFallbackSystemFactory
import sampler.abc.ABCModel
import com.typesafe.config.ConfigFactory
import sampler.io.Logging
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.actor.root.state.StateEngineService
import sampler.math.Statistics
import akka.pattern.ask
import akka.actor.ActorSystem
import java.util.concurrent.TimeUnit
import sampler.cluster.abc.parameters.ABCParameters

trait ABCMethod extends Logging{
	val stateEngineService: StateEngineService
	val system: ActorSystem

	def apply(model: ABCModel, params: ABCParameters) = {
		implicit val timeout = Timeout(params.cluster.futuresTimeoutMS, TimeUnit.MILLISECONDS)
		
		log.info("ABC configuration: {}",params)
		
		val modelRunner = AbortableModelRunner(model, params.cluster.parallelism)
		val targetParticleMemory = params.cluster.particleMemoryGenerations * params.job.numParticles
		
		val abcActor = system.actorOf(
				Props(new RootActor(model, params, modelRunner, stateEngineService)), 
				"abcrootactor"
		)
		
		import akka.pattern.ask
		val future = (abcActor ? Start(stateEngineService.init(model, params))).mapTo[Seq[model.ParameterSet]]
		val result = Await.result(future, Duration.Inf)
		
		if(params.cluster.terminateAtTargetGenerations){
			log.info("Terminating actor system")
			system.shutdown
		}
		
		result
	}
}

object ABCMethod extends ABCMethod with Logging{
	val stateEngineService =  new StateEngineService{
		val statistics = Statistics
		val toleranceCalculator = new ToleranceCalculator{}
		val weigher = new Weigher{}
	}
	
	val system = PortFallbackSystemFactory("ABCSystem")
}