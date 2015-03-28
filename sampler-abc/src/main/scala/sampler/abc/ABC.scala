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

package sampler.abc

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import com.typesafe.config.ConfigFactory
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import sampler.abc.actor.Report
import sampler.abc.actor.root.ABCActorImpl
import sampler.abc.config.ABCConfig
import sampler.cluster.PortFallbackSystemFactory
import sampler.io.Logging
import sampler.abc.actor.Start
import scala.concurrent.duration.Duration
import sampler.abc.core.Generation

trait ABCActors {
	val system: ActorSystem
	def entryPointActor[P](
			model: Model[P], 
			abcParams: ABCConfig, 
			reporting: Option[Report[P] => Unit]
	): ActorRef 
}

trait ABCActorsImpl extends ABCActors{
	val system = PortFallbackSystemFactory("ABC")
	
	def entryPointActor[P](
			model: Model[P], 
			config: ABCConfig, 
			reportAction: Option[Report[P] => Unit]) = {
		
		system.actorOf(
				Props(classOf[ABCActorImpl[P]], model, config, reportAction), 
				"root"
		)
	}
}

object ABC extends ABCActorsImpl with Logging {
	def apply[P](
			model: Model[P], 
			config: ABCConfig, 
			reporting: Report[P] => Unit
	): Seq[P] = apply(model, config, Some(reporting))
	
	def apply[P](
			model: Model[P], 
			config: ABCConfig, 
			reporting: Option[Report[P] => Unit] = None
	): Seq[P] = {
		
		log.info("Num generations: {}",config.job.numGenerations)
		log.info("Num particles: {}",config.job.numParticles)
		log.info("Num replicates: {}",config.job.numReplicates)
		log.info("Max particle retrys: {}",config.algorithm.maxParticleRetries)
		log.info("Particle chunk size: {}",config.algorithm.particleChunkSize)
		log.info("Mix rate {} MS",config.cluster.mixRateMS)
		log.info("Mix payload: {}",config.cluster.mixPayloadSize)
		log.info("Mix response threshold {} MS",config.cluster.mixResponseTimeoutMS)
		log.info("Futures timeout {} MS",config.cluster.futuresTimeoutMS)
		log.info("Particle memory generations: {}",config.cluster.particleMemoryGenerations)
		log.info("Terminate at target generations: {}",config.cluster.terminateAtTargetGenerations)
		log.info("Number of workers (router configured): {}", ConfigFactory.load().getInt("akka.actor.deployment./root/work-router.nr-of-instances"))
		
		val gen0 = Generation.init(model, config)
		val actor = entryPointActor(model, config, reporting)
		
		implicit val timeout = Timeout(config.cluster.futuresTimeoutMS, TimeUnit.MILLISECONDS)
		val future = (actor ? Start(gen0)).mapTo[Report[P]]
		val result = Await.result(future, Duration.Inf).posterior
		
		if(config.cluster.terminateAtTargetGenerations){
			log.info("Terminating actor system")
			system.shutdown
		}
		
		result
	}
}