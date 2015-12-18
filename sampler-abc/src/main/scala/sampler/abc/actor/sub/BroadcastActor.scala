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

package sampler.abc.actor.sub

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
import akka.cluster.MemberStatus
import sampler.math.Random
import akka.actor.Identify
import akka.actor.ActorIdentity
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._
import akka.actor.ActorSelection.toScala
import akka.cluster.ClusterEvent.ClusterDomainEvent
import sampler.abc.config.ABCConfig
import scala.concurrent.duration._
import akka.cluster.ClusterEvent.ReachableMember
import akka.cluster.ClusterEvent.UnreachableMember
import akka.cluster.Member
import sampler.data.DistributionBuilder
import sampler.abc.actor.main.MixPayload
import akka.cluster.ClusterEvent.ClusterDomainEvent
import akka.cluster.ClusterEvent.ClusterDomainEvent
import akka.cluster.ClusterEvent.ClusterDomainEvent

class BroadcastActor(abcParams: ABCConfig) extends Actor with ActorLogging{
	implicit val r = Random
	
	case class FoundNode(ref: ActorRef)

	val cluster = Cluster(context.system)

	override def preStart(): Unit = {
		cluster.subscribe(self, classOf[ClusterDomainEvent])
	}
	override def postStop(): Unit = {
		val cluster = Cluster(context.system)
		cluster.unsubscribe(self)
		log.info("Post stop")
	}
	
	import context._
	
	val selfAddress = cluster.selfAddress

	val config = ConfigFactory.load
	val testTimeout = Duration(abcParams.cluster.mixResponseTimeoutMS, MILLISECONDS)
	
	case class CheckPreMixingTests()
	case class PreMixingTest(msg: MixPayload[_], when: Long  = System.currentTimeMillis()){
		def durationSince = Duration(System.currentTimeMillis() - when, MILLISECONDS)
	}
	context.system.scheduler.schedule(1.second, testTimeout * 2 , self, CheckPreMixingTests)
	var preMixingTests = Map.empty[ActorRef, PreMixingTest]
	
	val nodes = collection.mutable.Set.empty[ActorRef]
	val recipientPath = Seq("user", "root", "receiver")
	
	def attemptWorkerHandshake(root: RootActorPath){
		val path = root / recipientPath
		log.debug("Attempting handshake with potential node: {}", path)
		context.actorSelection(path) ! Identify(None)
	}
	
	def nodeUp(member: Member){
		val rootPath = RootActorPath(member.address)
	  	if(member.address != selfAddress &&
	  		!nodes.exists(ref => ref.path.root == rootPath)
	  	){ 
	  		log.debug(s"Requesting handshake with ${member.address}")
	  		attemptWorkerHandshake(rootPath)
	  	} else {
	  		log.debug(s"NOT requesting handshake with self/known node: ${member.address}")
	  	}
	}
	
	def nodeDown(member: Member){
		log.warning(s"Node DOWN: $member")
		val down = nodes.filter(_.path.address == member.address)
		nodes --= down
	  	reportingActor ! NumWorkers(nodes.size)
	}
	
	def receive = {
		case state: CurrentClusterState => 
  		  	state.members.filter(_.status == MemberStatus.Up).foreach(m => 
  		  		attemptWorkerHandshake(RootActorPath(m.address))
  		  	)
		case MemberUp(member) => 					nodeUp(member)
		case ReachableMember(member) => 	nodeUp(member)
		case MemberRemoved(member, _) => 	nodeDown(member)
		case UnreachableMember(member) =>	nodeDown(member)
  		case ActorIdentity(None, Some(actorRef)) =>
  			// A handshake response
  			val remoteNode = actorRef
  			nodes += remoteNode
  			log.debug(s"Actor handshake identity from ${actorRef.path.address}")
  			reportingActor ! NumWorkers(nodes.size)
  			log.info("Handshake complete: {}",remoteNode)
  		case ActorIdentity(_: Long, Some(who)) =>
  			// A response to a pre-message test
  			if(preMixingTests.contains(who)){
  				val test = preMixingTests(who)
  				val responseTime = test.durationSince
  				val expired = responseTime > testTimeout
  				if(!expired){
  					log.debug("Pre-mix passed after {}, sent data to {}", responseTime, who)
	  				who ! test.msg
  				}
  				else log.warning("Pre-mix FAILED: {}>{} ms for {}", responseTime, testTimeout, who)
  			}
  			preMixingTests = preMixingTests - who
  		case CheckPreMixingTests =>
  			//Drop pending messages where the responsiveness test failed
  			preMixingTests = preMixingTests.filter{case (recipient, test) =>
  				val responseTime = test.durationSince
  				val expired = responseTime > testTimeout
  				if(expired) log.warning("Pre-mix FAILED: {}>{} ms for {}", responseTime, testTimeout, recipient)
  				!expired
  			}
  		case msg: MixPayload[_] =>
  			if(!nodes.isEmpty){
	  			val recipient = DistributionBuilder.uniform(nodes.toIndexedSeq).sample 
	  			if(!preMixingTests.contains(recipient)){
		  			val test = PreMixingTest(msg)
		  			preMixingTests = preMixingTests + (recipient -> test)
		  			recipient ! Identify(test.when)
	  			}
  			}
	}
	
	case class NumWorkers(n: Int)
	val reportingActor = context.actorOf(Props(new Actor with ActorLogging{
		val Tick = "timeToReport"
		import context.dispatcher
		import scala.concurrent.duration._
		
		var numWorkers: Option[Int] = None
		context.system.scheduler.schedule(5.second, 20.second, self, Tick)
		def receive = {
			case Tick => 
				numWorkers.foreach(n => log.info("There are {} remote nodes", n))
			case NumWorkers(n) => numWorkers = Some(n)
		}
	}))
}