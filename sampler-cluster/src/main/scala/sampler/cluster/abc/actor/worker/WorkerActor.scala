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

package sampler.cluster.abc.actor.worker

import java.rmi.UnexpectedException
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.actorRef2Scala
import sampler.run.DetectedAbortionException
import sampler.cluster.abc.actor.Abort
import sampler.cluster.abc.actor.Aborted
import sampler.cluster.abc.actor.Job
import sampler.cluster.abc.actor.TaggedAndScoredParameterSets
import sampler.cluster.abc.actor.Tagged
import scala.collection.parallel.CompositeThrowable
import scala.concurrent.duration._
import scala.concurrent.duration.DurationInt
import akka.actor.ActorRef
import sampler.abc.ABCModel
import sampler.math.Random

class WorkerActorImpl(val model: ABCModel) extends WorkerActor {
	implicit val random = Random
	val modelRunner = new ModelRunner{}
}

abstract class WorkerActor
		extends Actor
		with ActorLogging
		with ModelRunnerComponent {
	
	import context.become
		
	def receive = idle
	
	case class Contract(job: Job, client: ActorRef)

	def working(contract: Contract): Receive = {
		case newJob: Job =>
			log.debug("New job recieved from {}", sender)
			modelRunner.abort
			val newClient = sender
			become(waitingForAbortComfirmation(Some(Contract(newJob,newClient))))
			log.debug("Abort signal sent, waiting for confirmation")
		case Abort =>
			modelRunner.abort
			val newClient = sender
			become(waitingForAbortComfirmation(None))
		case Success(seq: Seq[_]) =>
			log.debug("Result received, sending to {}, starting another job", contract.client)
			contract.client ! TaggedAndScoredParameterSets(seq.map{Tagged(_)})
			startWork(contract.job)
		case Failure(exception) => 
			log.error("Model threw exception, but will be run again: {}", exception)
			startWork(contract.job)
		case Aborted => 
			become(idle)
		case msg => log.error("Unexpected message when 'working': {}",msg.getClass())
	}
	
	def waitingForAbortComfirmation(nextContract: Option[Contract]): Receive = {
		case newJob: Job =>
			if(nextContract.isDefined) 
				log.error("Overrieding previously queued work request (still waiting for last job to abort)")
			val newClient = sender
			become(waitingForAbortComfirmation(Some(Contract(newJob,newClient))))
		case out: Try[_] => 
			log.warning("Recieved result while waiting for abort comfirmation.  Assuming finished now", out)
			self ! Aborted
		case Aborted => 
			modelRunner.reset
			nextContract match{
				case Some(contract) =>
					log.debug("Abort confirmed, moving to next job")
					become(working(contract))
					startWork(contract.job)
				case None =>
					log.debug("Abort confirmed, becoming idle")
					become(idle)
			}
		case msg => log.error("Unexpected message when 'waiting for abort confirmation': {}",msg.getClass())
	}
	
	def idle(): Receive = {
		case job: Job =>
			startWork(job)
			val size = job.population.size
			val client = sender
			log.debug("Worker starting up at the request of {}", client)
			become(working(Contract(job, client)))
		case msg: Aborted => 
			throw new UnexpectedException("Not expected in idle state: "+msg)
		case Abort => //ignore
		case msg => log.error("Unexpected message when 'idle': {}",msg.getClass())
	}
	
	def startWork(job: Job){
		val me = self
		implicit val executionContext = context.system.dispatchers.lookup("sampler.work-dispatcher")
		Future{
			log.debug("Started work future")
			val result = modelRunner.run(job) 
			result match{
				case Failure(e: DetectedAbortionException) =>
					self ! Aborted
				case Failure(e: CompositeThrowable) if e.throwables.exists(_.isInstanceOf[DetectedAbortionException]) =>
					self ! Aborted
				case anythingElse =>
					me ! anythingElse
			}
		}
	}
}