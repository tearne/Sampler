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

package sampler.cluster.abc.distributed.actor

import java.rmi.UnexpectedException

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.actorRef2Scala
import sampler.cluster.abc.distributed.util.AbortableModelRunner
import sampler.run.DetectedAbortionException
import sampler.cluster.abc.distributed.actor.Messages._

class Worker(modelRunner: AbortableModelRunner) extends Actor with ActorLogging{
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
			parent ! LocalParameters(seq)
			startWork(currentJob)
			log.debug("Result sent to parent, started another job")
		case Failure(exception) => 
			log.warning("Model threw exception, but will be run again: {}", exception)
			startWork(currentJob)
		case Aborted => 
			become(idle)
		case msg => 
			throw new UnsupportedOperationException("Unexpected message "+msg)
	}
	
	def waitingForAbortComfirmation(nextJob: Option[Job]): Receive = {
		case newJob: Job =>
			log.warning("Overiding previous work request (still waiting for last job to abort)")
			become(waitingForAbortComfirmation(Some(newJob)))
		case out: Try[_] => 
			log.debug("Ignoring result, waiting for abort comfirmation")
		case Aborted => 
			log.debug("Aborted signal recieved")
			modelRunner.reset
			nextJob match{
				case Some(job) =>
					log.debug("Moving to next job")
					become(working(job))
					startWork(job)
				case None =>
					log.debug("Becoming idle")
					become(idle)
			}
	}
	
	def idle(): Receive = {
		case job: Job =>
			startWork(job)
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
				case anythingElse =>
					me ! anythingElse
			}
		}
	}
}