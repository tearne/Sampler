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

package sampler.cluster.abc.actor.root

import scala.concurrent.Future
import scala.concurrent.duration.DurationLong
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.actorRef2Scala
import akka.pattern.pipe
import sampler.cluster.abc.Scored
import sampler.cluster.abc.actor.Abort
import sampler.cluster.abc.actor.Job
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.actor.TaggedScoreSeq
import sampler.cluster.abc.config.ABCConfig
import sampler.cluster.abc.state.component.ToleranceCalculatorComponent
import sampler.cluster.abc.state.component.WeigherComponent
import sampler.data.Distribution
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import akka.actor.Cancellable
import sampler.cluster.abc.Model
import akka.actor.ActorRef
import sampler.cluster.abc.actor.Lenses
import sampler.cluster.abc.algorithm.AlgorithmComponentImpl
import sampler.cluster.abc.algorithm.Generation
import sampler.cluster.abc.algorithm.AlgorithmComponent
import akka.actor.FSM

class RootActorImpl[P](
		val model: Model[P],
		val config: ABCConfig
) extends RootActor[P]
		with ModelAndConfig[P]
		with ChildrenActorsComponent[P]
		with AlgorithmComponentImpl 
		with WeigherComponent
		with ToleranceCalculatorComponent 
		with StatisticsComponent 
		with Lenses {
	val childrenActors = new ChildrenActors{}
	val weigher = new Weigher{}
	val toleranceCalculator = new ToleranceCalculator{}
	val algorithm = new AlgorithmImpl{}
	val statistics = Statistics
	val random = Random
}

sealed trait Status
case object Idle extends Status
case object Gathering extends Status
case object Flushing extends Status
case object Finished extends Status

sealed trait Data
case object Uninitialized extends Data
case class Progress[P](generation: Generation[P], client: ActorRef) extends Data
case class Requestor(client: ActorRef) extends Data

abstract class RootActor[P]
		extends FSM[Status, Data] 
		with Actor 
		with ActorLogging
{
	this: ChildrenActorsComponent[P]
		with ModelAndConfig[P]
		with AlgorithmComponent
		with Lenses =>
	
	import model._
	import context._
	import childrenActors._
	
	type G = Generation[P]
	
//	case class Finished(generation: G)
//	case class NextGeneration(generation: G)
	case class FlushComplete(generation: G)
	
	case object MixNow
	
	var cancellable: Option[Cancellable] = None
	
	override def preStart{
		val mixPeriod = mixRateMS.get(config).milliseconds
		cancellable = Some(
			context.system.scheduler.schedule(mixPeriod * 10, mixPeriod, self, MixNow)
		)
	}
	override def postStop{
		cancellable.foreach(_.cancel)
	}

	implicit val r = Random
	
	startWith(Idle, Uninitialized)
	
	when(Idle) {
		case Event(s:Start[P], Uninitialized) =>
			val client = sender
			import s._
			workerRouter ! Job(generationZero.prevWeightsTable, config) //TODO Lens here
			goto(Gathering) using Progress(generationZero, client)
	}
	
	when(Gathering) {
		case Event(data: TaggedScoreSeq[P], p: Progress[P]) =>
			import p._
			val sndr = sender
			
			val newGen: G = algorithm.add(generation, data.seq, sndr, config)
			
			if(algorithm.numberAccumulated(newGen) < config.job.numParticles){
				goto(Gathering) using Progress(newGen, client)
				stay using p.copy(generation = newGen) 	//Continue gathering 
			} else {
				workerRouter ! Abort

				implicit val executionContext = context.system.dispatchers.lookup("sampler.work-dispatcher")
				
				Future{
					val flushedGen = algorithm.flushGeneration(newGen, config.job.numParticles)
					FlushComplete(flushedGen)
				}.pipeTo(self)
				goto(Flushing) using Requestor(client)
			}
		case  Event(MixNow, p: Progress[P]) => 
			import p._
			val payload = algorithm.buildMixPayload(generation, config)
			payload.foreach{message =>
				broadcaster ! message
			}
			stay
	}
	
	when(Flushing) {
		case Event(_: TaggedScoreSeq[P], _) => stay //Ignored
		case Event(MixNow, _) => stay				//Ignored
		case Event(FlushComplete(generation), Requestor(client)) =>
			import generation._
			val numGenerations = config.job.numGenerations
			log.info("Generation {}/{} complete, new tolerance {}", currentIteration, numGenerations, currentTolerance)
			
			if(currentIteration == numGenerations && config.cluster.terminateAtTargetGenerations){
				workerRouter ! Abort
				report(client, generation, true)
				log.info("Number of required generations completed and reported to requestor")
				goto(Idle) using Uninitialized
			} else {
				workerRouter ! Job(generation.prevWeightsTable, config)
				report(client, generation, false)
				goto(Gathering) using Progress(generation, client)
			}
	}
	
	def report(client: ActorRef, generation: Generation[P], finalReport: Boolean){
		client ! algorithm.buildReport(generation, config, finalReport)
	}
}