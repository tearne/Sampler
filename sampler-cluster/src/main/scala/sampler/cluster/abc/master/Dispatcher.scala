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

package sampler.cluster.abc.master

import akka.actor.ActorSystem
import scala.concurrent.duration._
import akka.util.Timeout
import scala.util.Try
import akka.actor.Props
import scala.concurrent.Await
import akka.actor.PoisonPill
import com.typesafe.config.ConfigFactory
import sampler.io.Logging
import akka.actor.actorRef2Scala
import sampler.cluster.abc.ABCJob
import akka.pattern.ask
import sampler.cluster.abc.IndexedJob

class Dispatcher(system: ActorSystem) extends Logging{
	
	implicit val timeout: Timeout = Try{
		val timeout = Timeout(ConfigFactory.load.getMilliseconds("sampler.futures-timeout"))
		log.info("Timeout from config: "+timeout)
		timeout
	}.toOption.getOrElse{
			val default = Timeout(5.minutes)
			log.warn("Default timeout of: "+default)
			default
	}
	
	import system.dispatcher
	
	val master = system.actorOf(Props[Master], name = "master")
	val jobIDIterator = Iterator.iterate(0)(_ + 1)
	
	def apply[P](job: ABCJob[P]): Seq[Try[P]] = {
		val accActor = system.actorOf(Props(classOf[AccumulatorActor], master))
		val wrappedJob = IndexedJob(job, jobIDIterator.next)
		val result = Await.result((accActor ? wrappedJob).mapTo[Seq[Try[P]]], timeout.duration)
		accActor ! PoisonPill
		result
	}
}