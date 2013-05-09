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

package sampler.run.akka.client

import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import scala.util.Try
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.MemberRemoved
import akka.actor.RootActorPath
import akka.cluster.Member
import akka.actor.ActorLogging
import akka.actor.actorRef2Scala
import akka.cluster.ClusterEvent.MemberRemoved
import sampler.run.akka.worker.Abort
import sampler.run.akka.worker.WorkConfirmed
import sampler.run.akka.worker.WorkRejected
import sampler.run.akka.worker.WorkerIdle

class RequestSupervisor extends Actor with ActorLogging{
	val confirmationTimeout = 1.second
	case class ConfirmationReminder()
	
	val cluster = Cluster(context.system)
  	override def preStart(): Unit = cluster.subscribe(self, classOf[MemberRemoved])
	override def postStop(): Unit = cluster.unsubscribe(self)
	
	def receive = {
		case Delegate(request,worker) => 
			log.info("Delegating request {}", request.jobID)
			worker ! request
			import context.dispatcher
			context.system.scheduler.scheduleOnce(confirmationTimeout, self, ConfirmationReminder)
			context.become(awaitingConfirmation(worker, request))
	}
	
	def awaitingConfirmation(worker: ActorRef, request: Request): Receive = {
		case Abort =>
			worker ! Abort
			context.stop(self)
		case WorkConfirmed =>
			log.info("Work confirmed")
			context.become(awaitingResults(worker, request)) 
		case ConfirmationReminder => 
			log.info("Confirmation reminder")
			failed(request)
		//TODO test
		case MemberRemoved(m) =>
			if(worker.path.address == m.address) {
				log.info(s"Worker lost at $m, resubmitting work to master")
				failed(request)
			}
		case WorkRejected => failed(request)
	}
	
	private def failed(request: Request){
		log.info("Failed, resubmitting request")
		context.parent.tell(request.job, request.requestor)
		context.stop(self)
	}
	
	private def clusterMemberLost(myWorker:ActorRef, member: Member, request: Request){
		val workerPath = Seq("user", "worker")
		val potentialWorker = context.actorFor(RootActorPath(member.address) / workerPath)
		if(myWorker == potentialWorker) {
			log.info("Cluster member lost during calculation")
			failed(request)
		}
	}
	
	def awaitingResults(worker: ActorRef, request: Request): Receive = {
		case MemberRemoved(m) => clusterMemberLost(worker, m, request)
		case Abort =>
			worker ! Abort
			context.stop(self)
		case result: Try[_] =>
			request.requestor ! result
			//TODO better mechanism for asking for new work after job completion 
			log.info("Job done")
			context.parent.tell(WorkerIdle, worker)
			context.stop(self)
	}
}