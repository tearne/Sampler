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
import com.jezhumble.javasysmon.JavaSysMon
import sampler.run.AbortableJob
import java.util.concurrent.atomic.AtomicBoolean

case class StatusRequest()
case class Status(numCPU: Int, load: Float, memFree: Float){
	override def toString() = f"Status(numCPU=$numCPU, load=$load%.2f, memFree=$memFree%.2f)"
}

case class IsWorkAvailable()
case class WorkIsAvailable()

//case class Job(f: () => Any){def apply() = f()}		//Job from a client

trait JobParameters
case class WorkerIsIdle()
case class JobID(requestor: ActorRef, allocId: Int)
case class Job(parameters: JobParameters, id: JobID)
case class JobDone(job: Job, result: Any)
case class JobConfirmed(work: Job)
case class JobRejected(work: Job)

//
// TODO only allow up to num CPU workers per physical node
//

//class WorkerBootable extends Bootable{
//	Try{
//		java.net.InetAddress.getLocalHost.getHostAddress
//	}match{
//		case Success(addr) => 
//			System.setProperty("akka.remote.netty.hostname", addr)
//			println("Using hostname "+addr)
//		case Failure(_) => println("Using config hostname")
//	}
//	val system = ActorSystem("ClusterSystem")
//	
//	def startup = system.actorOf(Props[Worker], name = "worker")
//	def shutdown = system.shutdown()
//}
//
//object Worker extends App{
//    if(args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
//	val system = ActorSystem("ClusterSystem")
//	system.actorOf(Props[MyWorker], name = "worker")
//}

trait WorkerComponent extends Actor with ActorLogging{
	def run: PartialFunction[Any, Option[Any]]
	
	import context.dispatcher	
	case class DoneWorking()
	val monitor = new JavaSysMon
	
	val masters = collection.mutable.Set.empty[ActorRef]
	
	val cluster = Cluster(context.system)
	override def preStart(): Unit = cluster.subscribe(self, classOf[UnreachableMember])
	override def postStop(): Unit = cluster.unsubscribe(self)
	
	def receive = idle
	
	def common: Receive = {
	  	case StatusRequest => 
	  	  	sender ! Status(
	  	  		monitor.numCpus(),
	  	  		monitor.cpuTimes.getIdleMillis.asInstanceOf[Float] / monitor.cpuTimes.getTotalMillis,
	  	  		monitor.physical.getFreeBytes.asInstanceOf[Float] / monitor.physical.getTotalBytes
	  	  	)
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
		case j: Job => 
			val master = sender
			Future{
				//TODO make jobs abortable by sending message to master
			  	val result = run(j.parameters)
				master ! JobDone(j, result)
			  	log.info("Work done, sending result to {}", master)
			  	DoneWorking
			}.pipeTo(self)	//Can't this be in the future?
		  	context.become(busy)
		  	sender ! JobConfirmed(j)
		  	if(!masters.contains(sender)) masters += sender
		  	log.info("Confirmed start of work {} to master {}", j, sender)
	}
	
	def busy: Receive = common orElse {
		case WorkIsAvailable => 
	  	  	log.info("Ignoring WorkAvailable from {} since busy", sender)
	  	  	if(!masters.contains(sender)) masters += sender
		case j: Job => 
			log.info("Rejecting Work since busy now")
			if(!masters.contains(sender)) masters += sender
	  	  	sender ! JobRejected(j)
		case DoneWorking => 
			log.info("Finished job, advertising for work to {}", masters)
		  	masters.foreach(_ ! IsWorkAvailable) //Looking for fastest response
			context.become(idle)
	}
}