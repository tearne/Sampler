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

package sampler.cluster.abc.slave

import sampler.cluster.actor.HostnameSetup
import sampler.io.Logging
import akka.actor.Props
import com.typesafe.config.ConfigFactory
import akka.actor.Actor
import akka.actor.ActorLogging
import sampler.cluster.abc.StatusRequest

class RootActor(builderFactory: => ParticleGenerator) extends Actor with ActorLogging{
	val n = ConfigFactory.load().getInt("sampler.node.workers-per")
	val numWorkers = if(n <= 0) Runtime.getRuntime().availableProcessors() else n

	(1 to numWorkers).foreach(i => context.actorOf(Props(new Worker(builderFactory))))
	
	log.info(s"Started $numWorkers workers")
	
	def receive = {
		case StatusRequest => 
			context.children.foreach{child => 
				child.forward(StatusRequest)
			}
			log.debug("Forwarded {} to {}", StatusRequest, context.children.size)
		case m => log.error("Unexpected message {} from {}",m, sender)
	}
}