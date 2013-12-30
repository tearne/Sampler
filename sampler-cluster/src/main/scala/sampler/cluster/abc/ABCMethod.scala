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
import sampler.cluster.abc.actor.root.RootActorImpl
import akka.actor.ActorRef
import sampler.cluster.abc.state.State

trait ABCMethod extends Logging{
	def run[P](abcActor: ActorRef, model: Model[P], abcParams: ABCParameters): Seq[P] = {
		implicit val timeout = Timeout(abcParams.cluster.futuresTimeoutMS, TimeUnit.MILLISECONDS)
		
		log.info("Num generations: {}",abcParams.job.numGenerations)
		log.info("Num particles: {}",abcParams.job.numParticles)
		log.info("Num replicates: {}",abcParams.job.numReplicates)
		log.info("Max particle retrys: {}",abcParams.algorithm.maxParticleRetries)
		log.info("Particle chunk size: {}",abcParams.algorithm.particleChunkSize)
		log.info("Mix rate {} MS",abcParams.cluster.mixRateMS)
		log.info("Mix payload: {}",abcParams.cluster.mixPayloadSize)
		log.info("Mix response threshold {} MS",abcParams.cluster.mixResponseTimeoutMS)
		log.info("Futures timeout {} MS",abcParams.cluster.futuresTimeoutMS)
		log.info("Particle memory generations: {}",abcParams.cluster.particleMemoryGenerations)
		log.info("Terminate at target generations: {}",abcParams.cluster.terminateAtTargetGenerations)
		log.info("Number of workers (router configured): {}", ConfigFactory.load().getInt("akka.actor.deployment./root/work-router.nr-of-instances"))
		
		
		import akka.pattern.ask
		val future = (abcActor ? Start(State.init(model, abcParams)))
				.mapTo[Seq[P]]
		Await.result(future, Duration.Inf)
	}
}

object ABCMethod extends ABCMethod with Logging{
	def apply[P](model: Model[P], abcParams: ABCParameters): Seq[P] = {
		val system = PortFallbackSystemFactory("ABC")
		
		val abcActor = system.actorOf(
			Props(classOf[RootActorImpl[P]],model, abcParams), 
			"root"
		)
		
		val result = run(abcActor, model, abcParams)
		
		if(abcParams.cluster.terminateAtTargetGenerations){
			log.info("Terminating actor system")
			system.shutdown
		}
		
		result
	}
}