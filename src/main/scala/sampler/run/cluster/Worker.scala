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

import akka.actor.{Actor, ActorSystem, Props, ActorLogging, ActorRef}
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.UnreachableMember
import scala.concurrent.Future
import akka.pattern.pipe
import akka.kernel.Bootable
import scala.util.Try
import scala.util.Success
import scala.util.Failure

case class DoesWorkerExist()
case class WorkerExists()

case class IsWorkAvailable()
case class WorkIsAvailable()

case class Job(f: () => Any){def apply() = f()}		//Job from a client

case class WorkerIsIdle()
case class JobID(requestor: ActorRef, allocId: Int)
case class Work(job: Job, jid: JobID)
case class WorkDone(work: Work, result: Any)
case class WorkConfirmed(work: Work)
case class WorkRejected(work: Work)

//
// TODO only allow up to num CPU workers per physical node
//

class WorkerBootable extends Bootable{
	Try{
		java.net.InetAddress.getLocalHost.getHostAddress
	}match{
		case Success(addr) => 
			System.setProperty("akka.remote.netty.hostname", addr)
			println("Using hostname "+addr)
		case Failure(_) => println("Using config hostname")
	}
	val system = ActorSystem("ClusterSystem")
	
	def startup = system.actorOf(Props[Worker], name = "worker")
	def shutdown = system.shutdown()
}

object WorkerApp extends App{
    if(args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
	val system = ActorSystem("ClusterSystem")
	system.actorOf(Props[Worker], name = "worker")
}

class Worker extends Actor with ActorLogging{
	import context.dispatcher	
	case class DoneWorking()
	
	val masters = collection.mutable.Set.empty[ActorRef]
	
	val cluster = Cluster(context.system)
	override def preStart(): Unit = cluster.subscribe(self, classOf[UnreachableMember])
	override def postStop(): Unit = cluster.unsubscribe(self)
	
	def receive = idle
	
	def common: Receive = {
	  	case DoesWorkerExist => 
	  	  	sender ! WorkerExists
	  	  	log.info("Confirmed I exist to {}", sender)
	  	case UnreachableMember(m) => 
	  		val addr = m.address
	  		masters.find(_.path.address == addr).foreach{ master =>
	  			masters -= master
	  			log.info("Removed master {}", master)
	  		}
	}
	
	def idle: Receive = common orElse {
		case WorkIsAvailable =>
			log.info("Work available from {}", sender)
			if(!masters.contains(sender)) masters += sender
			sender ! WorkerIsIdle
			log.info("Requested work from {}", sender)
		case w: Work => 
			val master = sender
			Future{
			  	master ! WorkDone(w, w.job())
			  	log.info("Work done, sending result to {}", master)
			  	DoneWorking
			}.pipeTo(self)	//Can't this be in the future?
		  	context.become(busy)
		  	sender ! WorkConfirmed(w)
		  	if(!masters.contains(sender)) masters += sender
		  	log.info("Confirmed start of work {} to master {}", w, sender)
	}
	
	def busy: Receive = common orElse {
		case WorkIsAvailable => 
	  	  	log.info("Ignoring WorkAvailable from {} since busy", sender)
	  	  	if(!masters.contains(sender)) masters += sender
		case w: Work => 
			log.info("Rejecting Work since busy now")
			if(!masters.contains(sender)) masters += sender
	  	  	sender ! WorkRejected(w)
		case DoneWorking => 
			log.info("Finished job, advertising for work to {}", masters)
		  	masters.foreach(_ ! IsWorkAvailable) //Looking for fastest response
			context.become(idle)
	}
}