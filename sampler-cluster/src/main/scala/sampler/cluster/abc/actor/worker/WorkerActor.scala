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
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.actor.TaggedScoredSeq
import sampler.math.Random
import sampler.cluster.abc.Scored
import scala.concurrent.Future
import sampler.cluster.abc.actor.Job
import sampler.cluster.abc.config.ABCConfig
import akka.actor.FSM
import akka.pattern.pipe
import sampler.cluster.abc.Weighted
import sampler.cluster.abc.actor.GenerateJob
import sampler.cluster.abc.actor.WeighJob
import sampler.cluster.abc.actor.Failed
import sampler.cluster.abc.actor.TaggedWeighedSeq
import sampler.cluster.abc.actor.Result

class WorkerActorImpl[P](val model: Model[P]) extends WorkerActor[P] {
	implicit val random = Random
	val modelRunner = new ModelRunner{}
	val weigher = new Weigher{}
}

sealed trait State
case object Idle extends State
case object Working extends State
case object AwaitingAbortConfirmation extends State

sealed trait Data
case object Uninitialised extends Data
case class Client(actorRef: ActorRef) extends Data
case class NextJob[P](job: Job[P], client: Client) extends Data

abstract class WorkerActor[P]
		extends Actor
		with FSM[State, Data]
		with ActorLogging
		with ModelRunnerComponent[P] 
		with WeigherComponent[P] {

	implicit val executionContext = context.system.dispatchers.lookup("sampler.work-dispatcher")
	
	startWith(Idle, Uninitialised)
	
	onTransition{
		case AwaitingAbortConfirmation -> _ => resetForNewWork
	}
	
	when(Idle){
		case Event(j: Job[P], Uninitialised) => 
			startWork(j)
			goto(Working) using Client(sender)
		case Event(Abort, Uninitialised) =>
			stay
	}
	
	when(Working){
		case Event(Abort, _) =>
			abortWork
			goto(AwaitingAbortConfirmation) using Uninitialised
		case Event(j: Job[P], _) => 
			abortWork
			goto(AwaitingAbortConfirmation) using NextJob(j, Client(sender))
		case Event(Success(result), Client(ref)) =>
			ref ! result
			goto(Idle) using Uninitialised
		case Event(Failure(result), Client(ref)) =>
			log.error(result.getMessage())
			ref ! Failed
			goto(Idle) using Uninitialised
	}
	
	when(AwaitingAbortConfirmation){
		case Event(j: Job[P], _) => 
			stay using NextJob(j, Client(sender))
		case Event(Abort, _) => 
			abortWork
			stay using Uninitialised
		
		case Event(Aborted, nj: NextJob[P]) =>
			startWork(nj.job)
			goto(Working) using nj.client
		case Event(Aborted, Uninitialised) =>	
			goto(Idle) using Uninitialised
		
		case Event(t: Try[_], _) => 
			self ! Aborted	//Don't care if success/fail since job was aborted
			stay
	}
	
	whenUnhandled{
		case Event(msg, _) =>
			log.error(s"Unhandled message ${msg.getClass} in state $stateName")
			stay
	}
	
	
	def abortWork {
		weigher.abort
		modelRunner.abort
	}
	
	def resetForNewWork {
		weigher.reset
		modelRunner.reset
	}
	
	def startWork(job: Job[P]){
		job match {
			case g: GenerateJob[P] => startGenerating(g)
			case w: WeighJob[P] => startWeighing(w)
		}
	}
	
	def startGenerating(gJob: GenerateJob[P]) {
		Future{
			log.debug("Generating")
			val result: Try[TaggedScoredSeq[P]] = modelRunner.run(gJob) 
			result match{
				case Failure(e: DetectedAbortionException) =>
					Aborted
				case Failure(e: CompositeThrowable) if e.throwables.exists(_.isInstanceOf[DetectedAbortionException]) =>
					Aborted
				case any =>
					any // Could be any success or failure other than the above
			}
		}.pipeTo(self)
	}
	
	def startWeighing(wJob: WeighJob[P]) {
		Future{
			log.debug("Weighing")
			val result: Try[TaggedWeighedSeq[P]] = weigher.run(wJob) 
			result match{
				case Failure(e: DetectedAbortionException) =>
					Aborted
				case Failure(e: CompositeThrowable) if e.throwables.exists(_.isInstanceOf[DetectedAbortionException]) =>
					Aborted
				case any =>
					any // Could be any success or failure other than the above
			}
		}.pipeTo(self)
	}
}