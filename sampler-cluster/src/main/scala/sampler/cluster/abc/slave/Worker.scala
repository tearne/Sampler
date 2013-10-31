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

package sampler.cluster.abc.slave

import akka.actor.Actor
import akka.actor.ActorLogging
import scala.concurrent.Future
import scala.util.Try
import scala.language.existentials
import akka.actor.actorRef2Scala
import akka.cluster.Cluster
import akka.cluster.ClusterEvent
import akka.actor.ActorRef
import sampler.cluster.abc.master.WorkAvailable
import sampler.cluster.abc.population.PopulationExecutor
import sampler.cluster.abc.ABCJob
import sampler.run.DetectedAbortionException
import scala.util.Failure
import scala.util.Success
import sampler.abc.ABCModel
import org.slf4j.LoggerFactory
import sampler.io.Logging
import scala.concurrent.Promise

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
class Worker(executor: PopulationExecutor) extends Actor with ActorLogging{
	case class Aborted()
	case class DoneWork()
	case class GotResult(work: IndexedJob, newResult: Try[ABCModel#Population])

	val cluster = Cluster(context.system)
	override def preStart(): Unit = cluster.subscribe(self, classOf[ClusterEvent.UnreachableMember])
	override def postStop(): Unit = cluster.unsubscribe(self)
	
	var future: Future[Unit] = Promise.successful().future
	var currentJob: Option[IndexedJob] = None
	var lastKnownMaster: Option[ActorRef] = None
	
	def receive = idle
	
	def idle: Receive = {
		case StatusRequest => {
			sender ! WorkerIdle
			val master = sender
			lastKnownMaster = Some(master)
		}
		case WorkAvailable => {
			log.debug("Work is available and I'm free")
			val master = sender
			lastKnownMaster = Some(master)
			sender ! WorkerIdle
		}
		case job: IndexedJob =>
			val requestor = sender
			log.info(s"Got request from $requestor")
			assert(currentJob.isEmpty)
			doWork(job, requestor)
	}
	
	def doWork(indexedJob: IndexedJob, requestor: ActorRef){
		val me = self
		context.become(busy(requestor))
		log.info("Starting work on job {}", indexedJob.id)
		import context._
		val fut = Future{
			val result: Try[ABCModel#Population] = executor.apply(indexedJob.job)
			result match{
				case Failure(e: DetectedAbortionException) =>
					log.info("Aborted future finished")
					me ! Aborted
					requestor ! result
				case result =>
					me ! GotResult(indexedJob, result)
					log.info("Future returned result", requestor)
			}
		}
		
		currentJob = Some(indexedJob)
		future = fut 
	}
	
	def busy(requestor: ActorRef): Receive = {
		case ClusterEvent.UnreachableMember(m) =>
			if(requestor.path.address == m.address) {
				log.warning(s"Detected requester unreachable "+m.address)
				self ! Abort
			}
		case StatusRequest => {
			sender ! WorkerBusy
			val master = sender
			lastKnownMaster = Some(master)
		}
		case Abort(id) => 
			if(currentJob.map(_.id == id).getOrElse(false)){
				log.info("============== Aborting ===============")
				executor.abort
				currentJob = None
				//Let the aborted future complete, then status will become idle automatically
			}
		case GotResult(work, newResult) => 
			if(currentJob.map(_.id == work.id).getOrElse(false)){
				requestor ! newResult
				log.info("Result sent to {}, starting antoher run", requestor)
				doWork(work, requestor)
				// Don't become idle, since another job might jump in, just start new job
				// Stay as busy with the same requestor reference
			} else {
				log.info("Ignoring stale result from job number {}")
				self ! Aborted
			}
		case Aborted => 
			log.info("Becoming idle")
			executor.reset
			context.become(idle)
			lastKnownMaster.foreach(_ ! WorkerIdle)
		case WorkAvailable => 
			val master = sender
			lastKnownMaster = Some(master)
			log.debug("Work is availbale but I'm busy")
	}
}