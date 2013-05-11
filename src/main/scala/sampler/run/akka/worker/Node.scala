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

package sampler.run.akka.worker

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props

//case class Status(numCPU: Int, load: Float, memFree: Float){
//	override def toString() = f"Status(numCPU=$numCPU, load=$load%.2f, memFree=$memFree%.2f)"
//}

class Node(runnerFactory: => Executor) extends Actor with ActorLogging{
	val numCores = Runtime.getRuntime().availableProcessors()
	(1 to numCores).foreach(i => context.actorOf(Props(new Worker(runnerFactory))))
	
	log.info(s"Started $numCores workers")
	
//	val monitor = new JavaSysMon
	
	def receive = {
//		case NodeCapability =>
//			sender ! Status(
//	  	  		monitor.numCpus(),
//	  	  		monitor.cpuTimes.getIdleMillis.asInstanceOf[Float] / monitor.cpuTimes.getTotalMillis,
//	  	  		monitor.physical.getFreeBytes.asInstanceOf[Float] / monitor.physical.getTotalBytes
//	  	  	)
		case StatusRequest => 
			context.children.foreach{child => 
				child.forward(StatusRequest)
				log.info("Forwarded {} from {} to {}", StatusRequest, sender, child)
			}
		case m => log.warning("Unexpected message {} from {}",m, sender)
	}
}