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
import scala.concurrent.duration.Duration

import com.typesafe.config.ConfigFactory

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import sampler.abc.actor.Report
import sampler.abc.actor.Start
import sampler.abc.actor.root.ABCActorImpl
import sampler.abc.config.ABCConfig
import sampler.abc.core.Generation
import sampler.abc.core.UseModelPrior
import sampler.cluster.PortFallbackSystemFactory
import sampler.io.Logging

trait ABCActors {
	val system: ActorSystem
	def entryPointActor[P](
		model: Model[P],
		abcParams: ABCConfig,
		reporting: Option[Report[P] => Unit]): ActorRef
}

trait ABCActorsImpl extends ABCActors {
	val system = PortFallbackSystemFactory("ABC")

	def entryPointActor[P](
		model: Model[P],
		config: ABCConfig,
		reportAction: Option[Report[P] => Unit]) = {

		system.actorOf(
			Props(classOf[ABCActorImpl[P]], model, config, reportAction),
			"root")
	}
}

object ABC extends ABCActorsImpl with Logging {
	def apply[P](
			model: Model[P],
			config: ABCConfig,
			reporting: Report[P] => Unit,
			initPop: Generation[P]): Seq[P] =
		apply(model, config, Some(reporting), initPop)

	def apply[P](
			model: Model[P],
			config: ABCConfig,
			reporting: Option[Report[P] => Unit] = None,
			initialPopulation: Generation[P] = UseModelPrior()): Seq[P] = {
		info(s"Num generations: ${config.job.numGenerations}")
		info(s"Num particles: ${config.job.numParticles}")
		info(s"Num replicates: ${config.job.numReplicates}")
		info(s"Max particle retrys: ${config.algorithm.maxParticleRetries}")
		info(s"Particle chunk size: ${config.algorithm.particleChunkSize}")
		info(s"Mix rate ${config.cluster.mixRateMS} MS")
		info(s"Mix payload: ${config.cluster.mixPayloadSize}")
		info(s"Mix response threshold ${config.cluster.mixResponseTimeoutMS} MS")
		info(s"Futures timeout ${config.cluster.futuresTimeoutMS} MS")
		info(s"Particle memory generations: ${config.cluster.particleMemoryGenerations}")
		info(s"Terminate at target generations: ${config.cluster.terminateAtTargetGenerations}")
		info(s"Number of workers (router configured): ${ConfigFactory.load().getInt("akka.actor.deployment./root/work-router.nr-of-instances")}")

		val actor = entryPointActor(model, config, reporting)

		implicit val timeout = Timeout(config.cluster.futuresTimeoutMS, TimeUnit.MILLISECONDS)
		val future = (actor ? Start(UseModelPrior())).mapTo[Report[P]]
		val result = Await.result(future, Duration.Inf).posterior
		//TODO unlimited timeout just for the future above?

		if (config.cluster.terminateAtTargetGenerations) {
			info("Terminating actor system")
			system.shutdown
		}

		result
	}
}