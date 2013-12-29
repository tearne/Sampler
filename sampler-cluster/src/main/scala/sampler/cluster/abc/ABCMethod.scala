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
import sampler.cluster.actor.PortFallbackSystemFactory
import sampler.abc.ABCModel
import com.typesafe.config.ConfigFactory
import sampler.io.Logging
import sampler.cluster.abc.actor.Start
import sampler.math.Statistics
import akka.pattern.ask
import akka.actor.ActorSystem
import java.util.concurrent.TimeUnit
import sampler.cluster.abc.parameters.ABCParameters
import sampler.cluster.abc.actor.root.RootActor
import sampler.cluster.abc.state.EncapsulatedState
import sampler.cluster.abc.actor.root.RootActorImpl
import akka.actor.ActorRef

trait ABCMethod extends Logging{
	def run(abcActor: ActorRef, model: ABCModel, params: ABCParameters): Seq[model.ParameterSet] = {
		implicit val timeout = Timeout(params.cluster.futuresTimeoutMS, TimeUnit.MILLISECONDS)
		
		log.info("Num generations: {}",params.job.numGenerations)
		log.info("Num particles: {}",params.job.numParticles)
		log.info("Num replicates: {}",params.job.numReplicates)
		log.info("Max particle retrys: {}",params.algorithm.maxParticleRetries)
		log.info("Particle chunk size: {}",params.algorithm.particleChunkSize)
		log.info("Mix rate {} MS",params.cluster.mixRateMS)
		log.info("Mix payload: {}",params.cluster.mixPayloadSize)
		log.info("Mix response threshold {} MS",params.cluster.mixResponseTimeoutMS)
		log.info("Futures timeout {} MS",params.cluster.futuresTimeoutMS)
		log.info("Particle memory generations: {}",params.cluster.particleMemoryGenerations)
		log.info("Terminate at target generations: {}",params.cluster.terminateAtTargetGenerations)
		log.info("Number of workers (router configured): {}", ConfigFactory.load().getInt("akka.actor.deployment./root/work-router.nr-of-instances"))
		
		
		import akka.pattern.ask
		val future = (abcActor ? Start(EncapsulatedState.init(model, params)))
				.mapTo[Seq[model.ParameterSet]]
		Await.result(future, Duration.Inf)
	}
}

object ABCMethod extends ABCMethod with Logging{
	def apply(model: ABCModel, params: ABCParameters): Seq[model.ParameterSet] = {
		val system = PortFallbackSystemFactory("ABC")
		
		val abcActor = system.actorOf(
			Props(classOf[RootActorImpl],model, params), 
			"root"
		)
		
		val result = run(abcActor, model, params)
		
		if(params.cluster.terminateAtTargetGenerations){
			log.info("Terminating actor system")
			system.shutdown
		}
		
		result
	}
}