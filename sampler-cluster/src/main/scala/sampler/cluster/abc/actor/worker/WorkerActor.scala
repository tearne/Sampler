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
import scala.collection.parallel.CompositeThrowable
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import sampler.cluster.abc.Model
import sampler.cluster.abc.actor.Abort
import sampler.cluster.abc.actor.Aborted
import sampler.cluster.abc.actor.Job
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.actor.TaggedScoreSeq
import sampler.math.Random
import sampler.run.DetectedAbortionException
import sampler.cluster.abc.Scored
import scala.concurrent.Future

class WorkerActorImpl[P](val model: Model[P]) extends WorkerActor[P] {
	implicit val random = Random
	val modelRunner = new ModelRunner{}
}

abstract class WorkerActor[P]
		extends Actor
		with ActorLogging
		with ModelRunnerComponent[P] {
	
	import context.become
	type T = Scored[P]
		
	def receive = idle
	val executionContext = context.system.dispatchers.lookup("sampler.work-dispatcher")
	
	case class Contract(job: Job[P], client: ActorRef)

	def working(contract: Contract): Receive = {
		case newJob: Job[P] =>
			log.debug("New job recieved from {}", sender)
			modelRunner.abort
			val newClient = sender
			become(waitingForAbortComfirmation(Some(Contract(newJob,newClient))))
			log.debug("Abort signal sent, waiting for confirmation")
		case Abort =>
			modelRunner.abort
			val newClient = sender
			become(waitingForAbortComfirmation(None))
		case Success(seq: Seq[T]) =>
			contract.client ! TaggedScoreSeq(seq.map{Tagged(_)})
			startWork(contract.job)
		case Failure(exception) => 
			log.error("Model threw exception, but will be run again: {}", exception)
			startWork(contract.job)
		case Aborted => 
			become(idle)
		case msg => log.error("Unexpected message from {} when 'working': {}",sender, msg.getClass())
	}
	
	def waitingForAbortComfirmation(nextContract: Option[Contract]): Receive = {
		case newJob: Job[P] =>
			if(nextContract.isDefined) 
				log.error("Override previously queued work request (still waiting for last job to abort)")
			val newClient = sender
			become(waitingForAbortComfirmation(Some(Contract(newJob,newClient))))
		case out: Try[_] => 
			log.warning("Recieved result while waiting for abort comfirmation.  Assuming finished now", out)
			self ! Aborted
		case Aborted => 
			modelRunner.reset
			nextContract match{
				case Some(contract) =>
					become(working(contract))
					startWork(contract.job)
				case None =>
					become(idle)
			}
		case msg => log.error("Unexpected message from {} when 'waiting for abort confirmation': {}",sender, msg.getClass())
	}
	
	def idle(): Receive = {
		case job: Job[P] =>
			startWork(job)
			val size = job.population.size
			val client = sender
			become(working(Contract(job, client)))
		case msg: Aborted => 
			throw new UnexpectedException("Not expected in idle state: "+msg)
		case Abort => //ignore
		case msg => log.error("Unexpected message when 'idle': {}",msg.getClass())
	}
	
	def startWork(job: Job[P]){
		val me = self
		Future{
			log.debug("Started work future")
			val result: Try[Seq[Scored[P]]] = modelRunner.run(job) 
			result match{
				case Failure(e: DetectedAbortionException) =>
					self ! Aborted
				case Failure(e: CompositeThrowable) if e.throwables.exists(_.isInstanceOf[DetectedAbortionException]) =>
					self ! Aborted
				case anythingElse =>
					me ! anythingElse
			}
		}(executionContext)
	}
}