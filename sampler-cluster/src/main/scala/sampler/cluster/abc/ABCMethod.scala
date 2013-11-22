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
import sampler.abc.ABCParameters
import com.typesafe.config.ConfigFactory
import sampler.io.Logging
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.actor.root.state.StateEngineService
import sampler.math.Statistics
import akka.pattern.ask
import akka.actor.ActorSystem
import java.util.concurrent.TimeUnit

trait ABCMethod extends Logging{
	val stateEngineService: StateEngineService
	
	val terminateAtTargetGeneration: Boolean
	val particleMemoryGenerations: Int
	
	val system: ActorSystem
	implicit val timeout: Timeout
	
	def apply(model: ABCModel, abcParameters: ABCParameters) = {
		val modelRunner = AbortableModelRunner(model)
		val targetParticleMemory = particleMemoryGenerations * abcParameters.numParticles
		
		val abcActor = system.actorOf(
				Props(new RootActor(model, abcParameters, modelRunner, stateEngineService)), 
				"abcrootactor"
		)
		
		import akka.pattern.ask
		val future = (abcActor ? Start(stateEngineService.init(model, abcParameters))).mapTo[Seq[model.ParameterSet]]
		val result = Await.result(future, Duration.Inf)
		
		if(terminateAtTargetGeneration){
			log.info("Terminating actor system")
			system.shutdown
		}
		
		result
	}
}

object ABCMethod extends ABCMethod with Logging{
	val config = ConfigFactory.load
	val terminateAtTargetGeneration = config.getBoolean("sampler.abc.terminate-at-target-generation")
	val particleMemoryGenerations = config.getInt("sampler.abc.particle-memory-generations")
	
	val stateEngineService =  new StateEngineService{
		val statistics = Statistics
		val toleranceCalculator = new ToleranceCalculator{}
		val weigher = new Weigher{}
		val numGenerationsMemory = particleMemoryGenerations
	}
	
	val system = PortFallbackSystemFactory("ABCSystem")
	import collection.JavaConversions._
	val milliSec: scala.Long = config.getMilliseconds("sampler.abc.system-timeout")
	implicit val timeout = Timeout(milliSec, TimeUnit.MILLISECONDS)
	
	log.info("ABC Method configuration, timeout {}, terminate at target {}, particle memory {}",
			timeout.toString,
			terminateAtTargetGeneration.toString,
			particleMemoryGenerations.toString
	)
}