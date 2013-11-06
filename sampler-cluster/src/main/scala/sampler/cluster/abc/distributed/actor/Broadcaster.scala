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

package sampler.cluster.abc.distributed.actor

import scala.concurrent.duration.DurationInt
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.RootActorPath
import akka.actor.actorRef2Scala
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.ClusterDomainEvent
import akka.cluster.ClusterEvent.CurrentClusterState
import akka.cluster.ClusterEvent.MemberRemoved
import akka.cluster.ClusterEvent.MemberUp
import akka.cluster.Member
import akka.cluster.MemberStatus
import sampler.data.Distribution
import sampler.math.Random
import sampler.cluster.abc.distributed.actor.Messages._
import akka.pattern.pipe
import akka.actor.Identify
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.ActorIdentity
import scala.concurrent.Await

class Broadcaster extends Actor with ActorLogging{
	val cluster = Cluster(context.system)
	implicit val r = Random
	
	case class FoundNode(ref: ActorRef)

	override def preStart(): Unit = cluster.subscribe(self, classOf[ClusterDomainEvent])
	override def postStop(): Unit = {
		cluster.unsubscribe(self)
		log.info("Post stop")
	}
	
	import context._
	
	val selfAddress = cluster.selfAddress
//	val myRemotePath = Await.result(system.actorSelection(self.path).resolveOne(1.second), 1.second).path.root
	
	case class AreYouThere()
	
	val nodes = collection.mutable.Set.empty[ActorRef]
	val nodePath = Seq("user", "abcrootactor")
	
	def attemptWorkerHandshake(root: RootActorPath){
		val path = root / nodePath
		log.info("Attempting handshake with potential node: {}", path)
		context.system.actorSelection(path) ! Identify(None)
	}
	
	def receive = {
		case state: CurrentClusterState => 
  		  	state.members.filter(_.status == MemberStatus.Up).foreach(m => 
  		  		attemptWorkerHandshake(RootActorPath(m.address))
  		  	)
  		case MemberUp(m) => 
  		  	val rootPath = RootActorPath(m.address)
  		  	log.info("{} is 'up'", m)
  		  	if(
  		  		m.address != selfAddress
	  			&&
	  			!nodes.exists(ref => ref.path.root == rootPath)
  		  	) 
  		  	attemptWorkerHandshake(rootPath)
  		case MemberRemoved(member, previousStatus) =>
  			val down = nodes.filter(_.path.address == member.address)
  			nodes --= down
  		  	log.info(s"Node down {}, previous status {}", down, previousStatus)
  		  	reportingActor ! NumWorkers(nodes.size)
  		case ActorIdentity(_, Some(actorRef)) =>
  			val remoteNode = actorRef
  			nodes += remoteNode
  			log.info("Handshake completed with node: {}",remoteNode)
  			log.info("node set = {}", nodes)
  		case msg: MixingMessage =>
  			if(!nodes.isEmpty){
	  			val recipient = Distribution.uniform(nodes.toIndexedSeq).sample 
	  			recipient ! msg
	  			log.info("Sent message {} to {}", msg.getClass(), recipient)
  			}
//  		case msg => log.warning("Unexpected message: "+msg)
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