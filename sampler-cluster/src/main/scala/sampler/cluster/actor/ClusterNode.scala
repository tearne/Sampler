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

package sampler.cluster.actor

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props
import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory
import scala.util.Try
import scala.util.{Success, Failure}
import sampler.io.Logging
import sampler.cluster.actor.worker.Executor
import sampler.cluster.actor.worker.Node
import sampler.io.Logging
import sampler.cluster.actor.util.HostnameSetup

class ClusterNode(executorFactory: => Executor) extends HostnameSetup with Logging{
	val system = PortFallbackSystem("ClusterSystem")
	val rootNodeActor = system.actorOf(Props(new Node(executorFactory)), name="workerroot")
	log.info("Started "+rootNodeActor)
}