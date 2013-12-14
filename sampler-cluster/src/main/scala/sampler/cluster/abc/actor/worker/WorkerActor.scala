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
import sampler.cluster.abc.actor.root.Tagged
import scala.collection.parallel.CompositeThrowable
import scala.concurrent.duration._
import scala.concurrent.duration.DurationInt

class Worker(modelRunner: AbortableModelRunner) 
		extends Actor 
		with ActorLogging
		 {
	import context._
	
	def receive = idle

	def working(currentJob: Job): Receive = {
		case newJob: Job =>
			log.debug("New job recieved")
			modelRunner.abort
			become(waitingForAbortComfirmation(Some(newJob)))
			log.debug("Abort signal sent, waiting for confirmation")
		case Abort =>
			modelRunner.abort
			become(waitingForAbortComfirmation(None))
		case Success(seq: Seq[_]) =>
			parent ! TaggedAndScoredParameterSets(seq.map{Tagged(_)})
			startWork(currentJob)
			log.debug("Result sent to parent, started another job")
		case Failure(exception) => 
			log.error("Model threw exception, but will be run again: {}", exception)
			startWork(currentJob)
		case Aborted => 
			become(idle)
		case msg => 
			throw new UnsupportedOperationException("Unexpected message "+msg)
	}
	
	def waitingForAbortComfirmation(nextJob: Option[Job]): Receive = {
		case newJob: Job =>
			log.error("Ignoring previous work request (still waiting for last job to abort)")
			become(waitingForAbortComfirmation(Some(newJob)))
		case out: Try[_] => 
			log.warning("Recieved result {} while waiting for abort comfirmation.  ASsuming finished now", out)
			self ! Aborted
		case Aborted => 
			log.debug("Aborted confirmation recieved")
			modelRunner.reset
			nextJob match{
				case Some(job) =>
					log.info("Moving to next job")
					become(working(job))
					startWork(job)
				case None =>
					log.info("Becoming idle")
					become(idle)
			}
		case msg => 
			log.error("Unexpected message when waiting for abort signal: {}", msg)
	}
	
	def idle(): Receive = {
		case job: Job =>
			startWork(job)
			val size = job.population.size
			become(working(job))
		case msg: Aborted => 
			throw new UnexpectedException("Not expected in idle state: "+msg)
	}
	
	def startWork(job: Job){
		log.debug("starting work")
		val me = self
		Future{
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