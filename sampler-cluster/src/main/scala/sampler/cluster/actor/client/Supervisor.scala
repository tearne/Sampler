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

package sampler.cluster.actor.client

import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import scala.util.Try
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.UnreachableMember
import akka.actor.RootActorPath
import akka.cluster.Member
import akka.actor.ActorLogging
import akka.actor.actorRef2Scala
import akka.cluster.ClusterEvent.MemberRemoved
import sampler.cluster.actor.worker.Abort
import sampler.cluster.actor.worker.WorkConfirmed
import sampler.cluster.actor.worker.WorkRejected
import sampler.cluster.actor.worker.WorkerIdle
import scala.util.Failure
import scala.util.Success

class Supervisor extends Actor with ActorLogging {
	val confirmationTimeout = 1.second
	case class ConfirmationReminder()
	
	val cluster = Cluster(context.system)
  	override def preStart(): Unit = cluster.subscribe(self, classOf[UnreachableMember])
	override def postStop(): Unit = cluster.unsubscribe(self)
	
	def receive = {
		case Delegate(request,worker) => 
			worker ! request
			import context.dispatcher
			context.system.scheduler.scheduleOnce(confirmationTimeout, self, ConfirmationReminder)
			context.become(awaitingConfirmation(worker, request))
			log.info("Assigned job {} to {}", request.jobID, worker)
	}
	
	def awaitingConfirmation(worker: ActorRef, request: Request): Receive = {
		case Abort =>
			worker ! Abort
			context.stop(self)
		case WorkConfirmed =>
			log.debug("Work confirmed")
			context.become(awaitingResults(worker, request)) 
		case ConfirmationReminder => 
			log.debug("Confirmation reminder")
			confirmationFailed(request)
		//TODO test
		case UnreachableMember(m) =>
			if(worker.path.address == m.address) {
				log.warning(s"Worker lost at $m, resubmitting work to master")
				confirmationFailed(request)
			}
		case WorkRejected => confirmationFailed(request)
	}
	
	private def confirmationFailed(request: Request){
		log.warning("Work confirmation failed, resubmitting request")
		context.parent.tell(request.job, request.requestor)
		context.stop(self)
	}
	
	private def clusterMemberLost(myWorker:ActorRef, member: Member, request: Request){
		val workerPath = Seq("user", "worker")
		val potentialWorker = context.actorFor(RootActorPath(member.address) / workerPath)
		if(myWorker == potentialWorker) {
			log.warning("Cluster member lost during calculation")
			confirmationFailed(request)
		}
	}
	
	def awaitingResults(worker: ActorRef, request: Request): Receive = {
		def behaviour: Receive = {
			case MemberRemoved(m) => clusterMemberLost(worker, m, request)
			case Abort =>
				worker ! Abort
				context.stop(self)
			case Failure(e) => 
				log.error(e, "Supervisor detected exception from worker")
				jobDone(Failure(e))
			case result: Success[_] =>
				jobDone(result)
		}

		def jobDone(result: Try[_]) {
			request.requestor ! result
			//TODO better mechanism for asking for new work after job completion 
			log.info("Job {} done", request.jobID)
			context.parent.tell(WorkerIdle, worker)
			context.stop(self)
		}
		
		behaviour
	}
	
}