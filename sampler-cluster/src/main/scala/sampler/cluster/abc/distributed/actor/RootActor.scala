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

import scala.Option.option2Iterable
import scala.concurrent.duration.DurationInt
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.actorRef2Scala
import sampler.cluster.abc.distributed.ABCParameters
import sampler.cluster.abc.distributed.ABCModel
import sampler.cluster.abc.distributed.actor.Messages.Abort
import sampler.cluster.abc.distributed.actor.Messages.NewScoredParameters
import sampler.cluster.abc.distributed.actor.Messages.Job
import sampler.cluster.abc.distributed.actor.Messages.Start
import sampler.cluster.abc.distributed.util.AbortableModelRunner
import sampler.cluster.abc.distributed.util.Helpers
import sampler.data.Distribution
import sampler.io.Logging
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._

class RootActor(val model: ABCModel, abcParams: ABCParameters, modelRunner: AbortableModelRunner) extends Actor with ActorLogging{
	import model._
	import context._
	
	val helpers = new Helpers with Logging with StatisticsComponent{
		val statistics = Statistics
	}
	
	val config = ConfigFactory.load
	val mixingMessageInterval = Duration(config.getMilliseconds("sampler.abc-mixing.rate"), MILLISECONDS)
	log.info("Mixing rate: {}", mixingMessageInterval)
	
	val broadcaster = context.actorOf(Props[Broadcaster], "broadcaster")
	val worker = context.actorOf(Props(new Worker(modelRunner)), "worker")
	context.system.scheduler.schedule(1.second, mixingMessageInterval, self, Mix)
	case class Mix()

	implicit val r = Random
	var inBox = Seq[Weighted]()
	var currentTolerance = Double.MaxValue
	var currentIteration = 0
	var currentWeightsTable: Map[ParameterSet,Double] = Map.empty
	
	def receive = idle
	
	log.info("READY")
	
	def idle: Receive = {
		case Start =>
			log.info("Starting")
			currentWeightsTable = {
				val generationZero = (1 to abcParams.numParticles).par.map(i => 
					Weighted(Scored(model.prior.sample(), Nil), 1.0)
				).seq
				worker ! Job(generationZero, abcParams)
				log.info("Sent init job to {}", worker)
				generationZero.map{weighed => (weighed.parameterSet, weighed.weight)}.toMap
			}
			val sndr = sender
			become(busy(sndr))
	}
	
	def busy(originalRequestor: ActorRef): Receive = {
		case NewScoredParameters(scored: Seq[Scored]) => 
			inBox = inBox ++ scored.map{scored =>
				helpers.weighScoredParameterSet(model)(scored, currentWeightsTable.toMap, currentTolerance)
			}.flatten
			if(sender.path.root == self.path.root) 
				log.info("Got {} particles from self, total = {}", scored.size, inBox.size)
			else 
				log.info("Got {} particles from {}, total = {}", scored.size, sender.path.root, inBox.size)
			if(inBox.size >= abcParams.numParticles) inBoxFull(originalRequestor)
		case Mix =>
			if(inBox.size > 0){
				val weightedParameters = Distribution.uniform(inBox.toIndexedSeq).until(_.size == abcParams.particleChunkSize).sample
				val scoredParameters = weightedParameters.map(_.scored)
				broadcaster ! NewScoredParameters(scoredParameters)
			}
	}
	
	def inBoxFull(originalRequestor: ActorRef) {
		log.info("InBox full, current it {}, required {}", currentIteration,abcParams.numGenerations)
		if(currentIteration == abcParams.numGenerations){
			originalRequestor ! inBox.map(_.parameterSet).take(abcParams.numParticles)
			worker ! Abort
		} else {
			val newPopulation = inBox
			val newTolerance = helpers.calculateNewTolerance(newPopulation, currentTolerance / 2)
			log.info("Generation {}, tolerance {}", currentIteration, newTolerance)
			worker ! Job(newPopulation, abcParams)
			log.info("Sent job to {}", worker)
			inBox = Seq.empty
			currentWeightsTable = helpers.consolidateTowWeightsTable(model)(newPopulation)
			currentTolerance = newTolerance
			currentIteration = currentIteration + 1
		}
	}
}