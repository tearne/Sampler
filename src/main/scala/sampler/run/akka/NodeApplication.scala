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

package sampler.run.akka

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props
import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory
import scala.util.Try
import scala.util.{Success, Failure}
import sampler.run.akka.worker.Executor
import sampler.run.akka.worker.StatusRequest
import sampler.run.akka.worker.Worker
import sampler.run.akka.worker.Node
import sampler.io.SLF4JLogging

class NodeApplication(executorFactory: => Executor) extends SLF4JLogging{
	if(ConfigFactory.load().getBoolean("cluster.inet-bind")){
		Try{
			java.net.InetAddress.getLocalHost.getHostAddress
		}match{
			case Success(addr) => 
				System.setProperty("akka.remote.netty.hostname", addr)
				log.info("Binding to local host address "+addr)
			case Failure(_) => 
				log.warn("Falling back to config hostname instead of inet host address")
		}
	} else log.info("Binding with hostname in config")
	ConfigFactory.invalidateCaches()
	
	val system = PortFallbackSystem.systemWithPortFallback("ClusterSystem")
	val rootNodeActor = system.actorOf(Props(new Node(executorFactory)), name="workerroot")
	log.info("Started "+rootNodeActor)
}