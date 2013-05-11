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

package sampler.run.actor.client

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import scala.concurrent.duration._
import akka.actor.Props
import akka.actor.actorRef2Scala
import sampler.run.actor.worker.Abort
import sampler.run.actor.worker.WorkerIdle
import sampler.run.actor.dispatch.Job
import sampler.run.actor.worker.Broadcaster
import scala.language.existentials
import sampler.run.actor.worker.Broadcast

case class Request(job: Job[_], requestor: ActorRef, jobID: Long)
case class WorkAvailable()
case class AbortAll()
case class Delegate(request: Request, worker: ActorRef)

class Master extends Actor with ActorLogging {
	case class BroadcastWorkAvailable()
	
	val broadcasterName = "broadcaster"
	val broadcaster: ActorRef = context.actorOf(Props[Broadcaster], broadcasterName)
	
	val requestQ = collection.mutable.Queue.empty[Request]
	val jobIDIterator = Iterator.iterate(0)(_ + 1)
	
	import context.dispatcher
	context.system.scheduler.schedule(1.seconds, 5.second, self, BroadcastWorkAvailable)
	
	override def postStop(){
		log.debug("Stopped")
	}
	
	def receive = {
		case job: Job[_] => 
			val requestor = sender
			val jobID = jobIDIterator.next
  		  	val newReq = Request(job, requestor, jobID)
  		  	requestQ += newReq 
  		  	log.info("New job request enqueued, id {}, |Q|={}", jobID, requestQ.size)
		case BroadcastWorkAvailable =>
			if(!requestQ.isEmpty) broadcaster ! Broadcast(WorkAvailable)
		case WorkerIdle =>
			log.info("Idle msg from {}.  {} Jobs in Q",sender, requestQ.size)
			val worker = sender
			if(!requestQ.isEmpty) 
				context.actorOf(Props[RequestSupervisor]) ! Delegate(requestQ.dequeue, worker)
		case AbortAll =>
			//All children other than the broadcaster are work monitors
			context.children.filter(_.path.name != broadcasterName).foreach(_ ! Abort)
			requestQ.clear
	}
}