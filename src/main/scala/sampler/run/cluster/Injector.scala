/*
 * Copyright (c) 2013 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
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

package sampler.run.cluster

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.cluster.Cluster
import akka.cluster.Member
import akka.actor.RootActorPath
import akka.cluster.ClusterEvent._
import akka.cluster.MemberStatus
import scala.collection._

case class NewWorker(worker: ActorRef)
case class WorkerDown(worker: ActorRef)
case class Broadcast(msg: Any)

class Injector extends Actor with ActorLogging{
	val cluster = Cluster(context.system)
  	override def preStart(): Unit = cluster.subscribe(self, classOf[ClusterDomainEvent])
	override def postStop(): Unit = cluster.unsubscribe(self)
	
	val workers = mutable.Set.empty[ActorRef]
	val workerPath = Seq("user", "worker")
	
	def attemptWorkerHandshake(m: Member){
	  val workerCandidate = context.actorFor(RootActorPath(m.address) / workerPath)
	  workerCandidate ! StatusRequest
	  log.debug("Attempting handshake with potential worker {}", workerCandidate)
	}
	
	def receive = {
		case state: CurrentClusterState => 
  		  	state.members.filter(_.status == MemberStatus.Up).foreach(attemptWorkerHandshake)
  		case MemberUp(m) => 
  		  	log.debug("Member {} is up", m)
  		  	attemptWorkerHandshake(m)
  		case s:Status =>
  		  	workers += sender
  		  	log.info(s"WORKER STATUS $sender: $s")
	  		log.info("Num workers = "+workers.size)  	
  		  	context.parent ! NewWorker(sender)
  		case UnreachableMember(m) => //Is this best thing to listen to?
  		  	val potentialWorker = context.actorFor(RootActorPath(m.address) / workerPath)
  		  	log.info("Downref = "+potentialWorker.path)
  		  	if(workers.contains(potentialWorker)){
  		  		workers -= potentialWorker
  		  		context.parent ! WorkerDown(potentialWorker)
  		  		log.info("Warned parent of unreachabler worker {}", potentialWorker)
  		  	}
		case Broadcast(msg) => 
			log.debug("Broadcasting {}, sender was {}", msg, sender)
			workers.foreach(_.forward(msg))
	}
}