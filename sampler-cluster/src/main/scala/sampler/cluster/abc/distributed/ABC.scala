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

package sampler.cluster.abc.distributed

import scala.concurrent.Await
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.Props
import sampler.cluster.abc.distributed.util.AbortableModelRunner
import sampler.cluster.abc.distributed.actor.RootActor
import sampler.cluster.abc.distributed.actor.Messages.Start
import sampler.cluster.actor.PortFallbackSystemFactory

object ABC {
	def run(model: ABCModel, abcParameters: ABCParameters) = {
		val system = PortFallbackSystemFactory("ABCSystem")
		val modelRunner = AbortableModelRunner(model)
		val abcActor = system.actorOf(
				Props(new RootActor(model, abcParameters, modelRunner)), 
				"abcrootactor"
		)
		
		import akka.pattern.ask
		implicit val timeout = Timeout(1.minute)
		val future = (abcActor ? Start).mapTo[Seq[model.ParameterSet]]
		val result = Await.result(future, 1.minute)
		
		system.shutdown
		
		result
	}
}