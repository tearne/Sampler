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

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import scala.concurrent.duration._
import akka.actor.Props
import akka.actor.actorRef2Scala
import sampler.cluster.abc.ABCJob
import sampler.cluster.abc.WorkAvailable
import sampler.cluster.abc.ClusterBusy
import sampler.cluster.abc.IndexedJob
import sampler.cluster.abc.WorkerIdle
import sampler.cluster.abc.AbortJob

class Master extends Actor with ActorLogging {
	case class BroadcastWorkAvailable()
	
	val broadcasterName = "broadcaster"
	val broadcaster: ActorRef = context.actorOf(Props[Broadcaster], broadcasterName)
	
	var currentRequest: Option[(IndexedJob, ActorRef)] = None
	
	import context.dispatcher
	context.system.scheduler.schedule(1.seconds, 10.second, self, BroadcastWorkAvailable)
	
	override def postStop(): Unit = {
		log.info("Post stop")
	}
	
	def receive = {
		case job: IndexedJob => 
			if(currentRequest.isDefined) {
				log.warning("Ignoring work from {} as cluster is busy", sender)
				sender ! ClusterBusy
			}
			else{
				val requestor = sender
				currentRequest = Some((job, requestor)) 
				log.info("Sending notification of new work")
				 broadcaster ! Broadcast(WorkAvailable)
			}
		case BroadcastWorkAvailable =>
			if(currentRequest.isDefined) {
				log.info("Routine notification of existing work")
				broadcaster ! Broadcast(WorkAvailable)
			}
		case WorkerIdle =>
			log.debug("Idle msg from {}", sender)
			val worker = sender
			currentRequest.foreach{case (job, requestor) => worker.tell(job, requestor)}
		case AbortJob(jobId) =>
			if(currentRequest.map(_._1.id == jobId).getOrElse(false)){
				log.info("Sending abort message")
				currentRequest = None
				broadcaster ! Broadcast(AbortJob(jobId))
			}else{
				log.warning("The cluster is not currently working on job {}, so cann't abort it.",jobId)
			}
	}
}