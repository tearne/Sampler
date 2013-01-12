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

package sampler.cluster

import akka.actor._
import akka.pattern.{pipe, ask}
import akka.cluster.Cluster
import akka.cluster.ClusterEvent._
import scala.concurrent.Future
import scala.util.{Success, Failure}
import scala.concurrent.duration._

object Worker extends App{
  if(args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
	val system = ActorSystem("ClusterSystem")
	val workerRef = system.actorOf(Props[WorkerActor], name = "worker")
  
	case class SeekNewWork
	
	class WorkerActor extends Actor with ActorLogging{
    	case class WorkComplete(result: Any)
    	var currentMaster: Option[ActorRef] = None 
		import context.dispatcher
    	
		def receive = idle
		
		//TODO
		//Reply with warning if handshake requests quickly follow each other?
		// or
		//Allow workers to have multiple masters?
		
		def idle: Receive = {
		  	case WorkerStatusRequest =>; 			log.info("Recieved handshake request") 
		  	  	currentMaster = Some(sender)
		  	  	sender ! Idle
			case r: Request => 
			  	Future{
			  		r.requestor ! r.work()
			  		WorkComplete
			  	}.pipeTo(self)	//Can't this be in the future?
			  	context.become(busy(r));			log.info("Now busy")
		}
  
  		def busy(request: Request): Receive = {
			case WorkerStatusRequest =>; 			log.info("Recieved handshake request") 
		  	  	currentMaster = Some(sender)
		  	  	sender ! Busy(request)
  			case WorkIsAvailable => 				log.info("Recieved work notification but busy. Ignoring.")
  			case WorkComplete => 
  			  	currentMaster.foreach{_ ! Idle}
  			  	context.become(idle);				log.info("Now idle")
  			case msg => 							log.warning("Message not supported while worker busy {}", msg)
  		}
	}
  
	//
	// TODO only allow up to num CPU workers per physical node
	//
}

object TestClient extends App{
	val system = ActorSystem("ClusterSystem")
	import scala.concurrent.Await
	import akka.pattern.ask
	implicit val askTimeout = akka.util.Timeout(5 minutes)
	implicit val ec = system.dispatcher
	
	val master = system.actorOf(Props[Master.MasterActor])
	
	val f = Future.sequence((1 to 10).map{i => 
		master ? Work(() => {Thread.sleep(5000); i.toString})
	}) mapTo manifest[IndexedSeq[String]]
	
	val res = Await.result(f, askTimeout.duration)
	println(res)
}

case class Work(f: () => Any){
	def apply() = f()
}
case class Request(requestor: ActorRef, work: Work)
case class WorkIsAvailable()

case class WorkerStatusRequest()
trait WorkerStatus
case class Busy(request: Request) extends WorkerStatus
case class Idle() extends WorkerStatus

object Master extends App{
	val system = ActorSystem("ClusterSystem")
	val workerRef = system.actorOf(Props[MasterActor])
  
	class MasterActor extends Actor with ActorLogging{
		import akka.cluster.{Member, MemberStatus}

		val workerChildPath = Seq("user", "worker")
		
		val cluster = Cluster(context.system)
		override def preStart(): Unit = cluster.subscribe(self, classOf[ClusterDomainEvent])
		override def postStop(): Unit = cluster.unsubscribe(self)

		val requestQ = collection.mutable.Queue.empty[Request]
		val workerStatus = collection.mutable.Map.empty[ActorRef, Option[Request]]
	  
		def receive = {
	  		case state: CurrentClusterState => 
	  		  	state.members.filter(_.status == MemberStatus.Up).foreach(attemptHandshake)
	  		case MemberUp(m) => 
	  		  	attemptHandshake(m)
	  		//Is this best thing to listen to?
	  		case UnreachableMember(m) =>
	  		  	val downRef = context.actorFor(RootActorPath(m.address) / workerChildPath)
	  		  	log.info("Downref = "+downRef.path)
	  		  	if(workerStatus.contains(downRef)){
		  		  	workerStatus(downRef).foreach{request =>
		  		  		self ! request;				log.info("Work from {} rescheduled path {}", downRef, downRef.path)
		  		  		//TODO if the node is still alive it may complete the job and
		  		  		// attempt to send the result.  Might need a way to stop it.
		  		  	}								
		  		  	workerStatus -= downRef
		  		  	log.info("{} workers remain", workerStatus.size)
	  		  	}
	  		case Idle =>
	  		  	workerStatus += (sender -> None)
	  		  	if(!requestQ.isEmpty) assignWork(sender)
//	  		  	log.info("Handshake complete, now {} workers in pool", workerStatus.size)
	  		case Busy(request) =>
	  		  	workerStatus += (sender -> Some(request))
	  		case work: Work => 
	  			requestQ.enqueue(Request(sender,work))	//Queue up new work
	  			notifyWorkAvailable					//Tell idle workers to come and get it
		}
		
		def workerIdle(actor: ActorRef){
			workerStatus += (actor -> None)	
		}
		
		def attemptHandshake(m: Member){
			log.info("Attempting handshake for "+m.address)
			context.actorFor(RootActorPath(m.address) / workerChildPath) ! WorkerStatusRequest
		}
		
		def notifyWorkAvailable(){
		  if(!requestQ.isEmpty) workerStatus.foreach{
		    case (worker, None) => worker ! WorkIsAvailable
		    case _ => //Worker is believed to be busy
		  }
		}
		
		def assignWork(worker: ActorRef){
			val request = requestQ.dequeue 
			worker ! request
			workerStatus += worker -> Some(request)
		}
	}
}