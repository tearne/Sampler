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

package sampler.cluster.abc.distributed

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.cluster.Cluster
import akka.actor.ActorRef
import akka.cluster.ClusterEvent._
import akka.cluster.Member
import akka.actor.RootActorPath
import akka.cluster.MemberStatus
import akka.actor.actorRef2Scala
import akka.cluster.ClusterEvent.ClusterDomainEvent
import akka.actor.Props
import akka.cluster.ClusterEvent.ClusterDomainEvent
import scala.concurrent.duration.DurationInt
import sampler.cluster.abc.WorkerBusy
import sampler.cluster.abc.WorkerIdle
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await
import sampler.data.Distribution
import sampler.math.Random
import akka.actor.Identify
import scala.util.Success
import scala.util.Failure

case class Broadcast(msg: Any)
case class RandomSend(msg: Any)
case class FoundNode(ref: ActorRef)

class Broadcaster extends Actor with ActorLogging{
	val cluster = Cluster(context.system)
	implicit val r = Random

	override def preStart(): Unit = cluster.subscribe(self, classOf[ClusterDomainEvent])
	override def postStop(): Unit = {
		cluster.unsubscribe(self)
		log.info("Post stop")
	}
	
	import context._
	
	val nodes = collection.mutable.Set.empty[ActorRef]
	val nodePath = Seq("user", "abcactor")
	
	def attemptWorkerHandshake(m: Member){
		val path = RootActorPath(m.address) / nodePath
		
		val future = context.system.actorSelection(path).resolveOne(1.second)
		log.info("Attempting handshake with potential node: {}", path)
		future.onSuccess{
			case ref => 
				self ! FoundNode(ref)
				log.info("Handshake completed with node: {}",ref)
		}
		
	}
	
	def receive = {
		case state: CurrentClusterState => 
  		  	state.members.filter(_.status == MemberStatus.Up).foreach(attemptWorkerHandshake)
  		case MemberUp(m) => 
  		  	log.debug("Member {} is up", m)
  		  	attemptWorkerHandshake(m)
  		case MemberRemoved(member, previousStatus) =>
  			val down = nodes.filter(_.path.address == member.address)
  			nodes --= down
  		  	log.info(s"Node down {}, previous status", down, previousStatus)
  		  	reportingActor ! NumWorkers(nodes.size)
  		case FoundNode(ref) =>
  			nodes += ref
  		case Broadcast(msg) => 
			log.info("Broadcasting {} to {} known nodes", msg, nodes.size)
			nodes.foreach(_.forward(msg))
  		case RandomSend(msg) =>
  			if(nodes.size > 1){
	  			val recipient = Distribution.uniform(nodes.-(context.parent).toIndexedSeq).sample 
	  			recipient ! msg
	  			log.info("Sent message {} to {}", msg, recipient)
  			}
	}
	
	case class NumWorkers(n: Int)
	val reportingActor = context.actorOf(Props(new Actor with ActorLogging{
		val Tick = "timeToReport"
		import context.dispatcher
		import scala.concurrent.duration._
		
		var numWorkers: Option[Int] = None
		context.system.scheduler.schedule(1.second, 10.second, self, Tick)
		def receive = {
			case Tick => 
				numWorkers.foreach(n => log.info("{} workers in the cluster", n))
				numWorkers = None
			case NumWorkers(n) => numWorkers = Some(n)
		}
	}))
}