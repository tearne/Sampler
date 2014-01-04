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
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.FSM
import akka.actor.actorRef2Scala
import akka.pattern.pipe
import sampler.cluster.abc.Model
import sampler.cluster.abc.actor.Abort
import sampler.cluster.abc.actor.Job
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.actor.TaggedScoreSeq
import sampler.cluster.abc.algorithm.AlgorithmComponent
import sampler.cluster.abc.algorithm.AlgorithmComponentImpl
import sampler.cluster.abc.algorithm.Generation
import sampler.cluster.abc.algorithm.component.ToleranceCalculatorComponent
import sampler.cluster.abc.algorithm.component.WeigherComponent
import sampler.cluster.abc.config.ABCConfig
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent

class RootActorImpl[P](
		val model: Model[P],
		val config: ABCConfig
) extends RootActor[P]
		with ChildrenActorsComponent[P]
		with AlgorithmComponentImpl 
		with WeigherComponent
		with ToleranceCalculatorComponent 
		with StatisticsComponent 
		with GettersComponent {
	val childActors = new ChildActors{}
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

sealed trait Data
case object Uninitialized extends Data
case class Progress[P](generation: Generation[P], actorRef: ActorRef) extends Data
case class Client(actorRef: ActorRef) extends Data

abstract class RootActor[P]
		extends FSM[Status, Data] 
		with Actor 
		with ActorLogging
{
	this: ChildrenActorsComponent[P]
		with AlgorithmComponent
		with GettersComponent =>
	
	val config: ABCConfig
	val model: Model[P]
			
	import model._
	import context._
	import childActors._
	
	type G = Generation[P]
	
	case class FlushComplete(generation: G)
	case object MixNow
	
	var cancellable: Option[Cancellable] = None
	
	override def preStart{
		val mixPeriod = getters.getMixRateMS(config).milliseconds
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
			val sndr = sender
			val newGen: G = algorithm.add(p.generation, data.seq, sndr, config)
			
			if(algorithm.numberAccumulated(newGen) < config.job.numParticles){
				goto(Gathering) using Progress(newGen, p.actorRef)
				stay using p.copy(generation = newGen) 	//Continue gathering 
			} else {
				workerRouter ! Abort

				implicit val executionContext = context.system.dispatchers.lookup("sampler.work-dispatcher")
				Future{
					val flushedGen = algorithm.flushGeneration(newGen, config.job.numParticles)
					FlushComplete(flushedGen)
				}.pipeTo(self)
				
				goto(Flushing) using Client(p.actorRef)
			}
		case Event(MixNow, p: Progress[P]) => 
			val payload = algorithm.buildMixPayload(p.generation, config)
			payload.foreach{message =>
				broadcaster ! message
			}
			
			stay
	}
	
	when(Flushing) {
		case Event(_: TaggedScoreSeq[P], _) => stay //Ignored
		case Event(MixNow, _) => stay				//Ignored
		case Event(FlushComplete(generation), Client(client)) =>
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