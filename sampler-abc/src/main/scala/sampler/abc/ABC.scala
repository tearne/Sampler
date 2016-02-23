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

import java.util.concurrent.TimeUnit.MILLISECONDS
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import com.typesafe.config.ConfigFactory
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import sampler.abc.actor.main.MainActorImpl
import sampler.cluster.PortFallbackSystemFactory
import sampler.io.Logging
import sampler.abc.actor.main.Start

trait ABCActors {
	val system: ActorSystem
	def entryPointActor[P](
		model: Model[P],
		config: ABCConfig,
		generationHandler: Option[Population[P] => Unit]): ActorRef
}

trait ABCActorsImpl extends ABCActors {
	val system = PortFallbackSystemFactory("ABC")

	def entryPointActor[P](
		model: Model[P],
		config: ABCConfig,
		generationHandler: Option[Population[P] => Unit]) = {

		system.actorOf(
			Props(classOf[MainActorImpl[P]], model, config, generationHandler),
			"root")
	}
}

object ABC extends ABCActorsImpl with Logging {
	def apply[P](
			model: Model[P],
			config: ABCConfig): Population[P] =
		apply(model, config, None, UseModelPrior[P]())
		
	def apply[P](
			model: Model[P],
			config: ABCConfig,
			genHandler: Population[P] => Unit): Population[P] =
		apply(model, config, Some(genHandler), UseModelPrior())
	
	def apply[P](
			model: Model[P],
			config: ABCConfig,
			genHandler: Option[Population[P] => Unit] = None,
			initialPopulation: Generation[P] = UseModelPrior()): Population[P] = {
		info("Running with config: "+config.render)

		val actor = entryPointActor(model, config, genHandler)

		implicit val timeout = Timeout(config.futuresTimeoutMS, MILLISECONDS)
		val future = (actor ? Start(UseModelPrior())).mapTo[Population[P]]
		val result = Await.result(future, Duration.Inf)
		//TODO unlimited timeout just for the future above?

		if (config.terminateAtTargetGen) {
			info("Terminating actor system")
			system.shutdown
		}

		result
	}
}