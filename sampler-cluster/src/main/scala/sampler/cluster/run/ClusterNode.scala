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

package sampler.cluster.run

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props
import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory
import scala.util.Try
import scala.util.{Success, Failure}
import sampler.io.Logging
import sampler.io.Logging
import sampler.cluster.run.slave.Executor
import sampler.cluster.actor.PortFallbackSystemFactory
import sampler.cluster.run.slave.Node

class ClusterNode(executorFactory: => Executor) extends Logging{
	val system = PortFallbackSystemFactory("ClusterSystem")
	val rootNodeActor = system.actorOf(Props(new Node(executorFactory)), name="workerroot")
	log.info("Started "+rootNodeActor)
}