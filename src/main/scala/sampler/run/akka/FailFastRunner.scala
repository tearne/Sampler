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

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import akka.actor.ActorDSL.Act
import akka.actor.ActorDSL.actor
import akka.actor.ActorSystem
import akka.actor.PoisonPill
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask
import akka.util.Timeout
import sampler.run.Job
import sampler.run.JobRunner
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef

case class Batch[T](jobs: Seq[Job[T]]){
	val size = jobs.size
}

class FailFastRunner(val system: ActorSystem) extends JobRunner{
	implicit val timeout = Timeout(5.minutes)
	import system.dispatcher
	
	def apply[T](jobs: Seq[Job[T]]): Seq[Try[T]] = {
		val ff = system.actorOf(Props[FailFastActor])
		Await.result((ff ? Batch(jobs)).mapTo[Seq[Try[T]]], timeout.duration)
	}
}

class FailFastActor extends Actor with ActorLogging{
	val results = collection.mutable.Buffer[Try[Any]]()
	val master = context.actorOf(Props[Master], name = "master")
	
	def receive = startup
	
	def startup: Receive = {
		case b: Batch[_] => 
			val client = sender
			b.jobs.foreach(master ! _)
			context.become(running(client, b.size))
	}
	
	def running(client: ActorRef, numJobs: Int): Receive = {
		case s: Success[_] => 
			results += s
			if(results.size == numJobs) { 
				client ! results
				self ! PoisonPill
			}
		case f: Failure[_] => 
			log.warning("Sending abort to "+master)
			master ! AbortAll
			results += f
			client ! results.toSeq
			self ! PoisonPill
	}
}