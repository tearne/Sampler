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
import scala.concurrent.Future
import scala.util.Try
import akka.actor.actorRef2Scala
import sampler.run.akka.worker.Executor
import sampler.run.akka.client.WorkAvailable
import sampler.run.akka.client.Request
import sampler.run.ActorJob

case class Abort()
case class StatusRequest()
case class WorkerBusy()
case class WorkerIdle()
case class WorkConfirmed()
case class WorkRejected()

//object WorkerApp extends App with HostnameSetup{
//	if(args.size == 1){
//		System.setProperty("akka.remote.netty.port", args(0))
//		ConfigFactory.invalidateCaches()
//	}
//	val system = ActorSystem.withPortFallback("ClusterSystem")
//	system.actorOf(Props(new WorkerBase(new TestDomainWorker)), name = "workerroot")
//}

/*
 * Keep the worker simple.  It just responds to status requests
 * and switches between idle and busy states, with the option 
 * of aborting work
 */
class Worker(runner: Executor) extends Actor with ActorLogging{
	case class Done()
	
	def receive = idle
	
	def idle: Receive = {
		case StatusRequest => sender ! WorkerIdle
		case WorkAvailable => sender ! WorkerIdle
		case request: Request =>
			log.info("Got request "+request.job)
			doWork(request.job)
	}
	
	def doWork(job: ActorJob[_]){
		val sndr = sender
		val me = self
		context.become(busy)
		import context._
		Future{
			val result = Try(runner(job))
			me ! Done
			sndr ! result
		}
		sndr ! WorkConfirmed
	}
	
	def busy: Receive = {
		case StatusRequest => sender ! WorkerBusy
		case Abort => 
			log.info("Aborting")
			runner.abort
			//TODO how do we know that it's actually aborted?
			context.become(idle)
		case Done => 
			log.info("Becoming idle")
			context.become(idle)
		case Request => log.warning("Received request when busy, ignoring")
		case WorkAvailable => //Ignore
		case msg => log.warning("unknown msg "+msg.toString)
	}
}