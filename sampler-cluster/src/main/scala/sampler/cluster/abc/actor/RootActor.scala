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

package sampler.cluster.abc.actor

import scala.Option.option2Iterable
import scala.concurrent.duration.DurationInt
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import sampler.abc.ABCParameters
import sampler.abc.ABCModel
import sampler.cluster.abc.util.AbortableModelRunner
import sampler.cluster.abc.util.Helpers
import sampler.io.Logging
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._
import akka.cluster.Cluster
import scala.collection.SortedSet
import akka.actor.Props

//A value wrapped together with its origin
case class Tagged[T](value: T, originCode: Long)

class RootActor(val model: ABCModel, abcParams: ABCParameters, modelRunner: AbortableModelRunner) extends Actor with ActorLogging{
	import model._
	import context._
	
	val helpers = new Helpers with Logging with StatisticsComponent{
		val statistics = Statistics
	}
	
	val myRemoteAddress = Cluster(system).selfAddress
	
	val config = ConfigFactory.load
	val terminateAtTargetGeneration = config.getBoolean("sampler.abc.terminate-at-target-generation")
	val mixingMessageInterval = Duration(config.getMilliseconds("sampler.abc.mixing.rate"), MILLISECONDS)
	log.info("Mixing rate: {}", mixingMessageInterval)
	
	val broadcaster = context.actorOf(Props[Broadcaster], "broadcaster")
	val worker = context.actorOf(Props(new Worker(modelRunner)), "worker")
	context.system.scheduler.schedule(1.second, mixingMessageInterval, self, Mix)
	case class Mix()

	implicit val r = Random
	type Origin = ActorRef
	
	var inBox = Set[Tagged[Weighted]]()
	var particlesAlreadySeen: SortedSet[Long] = SortedSet.empty
	
	var currentTolerance = Double.MaxValue
	var currentIteration = 0
	var currentWeightsTable: Map[ParameterSet,Double] = Map.empty
	
	def receive = idle
	
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
	
	def busy(client: ActorRef): Receive = {
		case LocalParameters(localScoreds: Seq[Scored]) =>
			val weighedAndTagged = localScoreds.map{scored =>
					helpers.filterAndWeighScoredParameterSet(model)(scored, currentWeightsTable.toMap, currentTolerance)
						.map{weighted => Tagged(weighted, System.currentTimeMillis())}
				}.flatten
			inBox = inBox ++ weighedAndTagged
			particlesAlreadySeen = particlesAlreadySeen ++ weighedAndTagged.map(_.originCode)
			if(particlesAlreadySeen.size > 10 * abcParams.numParticles){
				particlesAlreadySeen = particlesAlreadySeen.drop(abcParams.numParticles)
			}
			log.info("Generated {} samples, kept {}, accumulated {}", localScoreds.size, weighedAndTagged.size, inBox.size)
			if(inBox.size >= abcParams.numParticles) inBoxFull(client)
			
		case RemoteParameters(remoteScoreds: Seq[Tagged[Scored]]) =>
			val weighedAndTagged = remoteScoreds.view
				.filter(scored => !particlesAlreadySeen.contains(scored.originCode))
				.map{case Tagged(scored, address) =>
					helpers.filterAndWeighScoredParameterSet(model)(scored, currentWeightsTable.toMap, currentTolerance)
						.map(weighted => Tagged(weighted, address))
				}.flatten
			inBox = inBox ++ weighedAndTagged // InBox is a set, so duplicates wont be added again
			particlesAlreadySeen = particlesAlreadySeen ++ weighedAndTagged.map(_.originCode)
			if(particlesAlreadySeen.size > 10 * abcParams.numParticles){
				particlesAlreadySeen = particlesAlreadySeen.drop(abcParams.numParticles)
			}
			log.info("Received {} particles, kept {}, accumulated {}", remoteScoreds.size, weighedAndTagged.size, inBox.size)
			if(inBox.size >= abcParams.numParticles) inBoxFull(client)
				
		case Mix =>
			if(inBox.size > 0){
				// Send entire inBox
				broadcaster ! RemoteParameters(inBox.toSeq.map{case Tagged(weighted, origin) =>
					Tagged(weighted.scored, origin)
				})
			}
	}
	
	def inBoxFull(client: ActorRef) {
		log.info("Generation {}/{} complete", currentIteration, abcParams.numGenerations)
		if(currentIteration == abcParams.numGenerations){
			client ! inBox.toSeq.map(_.value.parameterSet).take(abcParams.numParticles)
			log.info("Number of required generations completed and reported to requestor")
			
			if(terminateAtTargetGeneration){
				worker ! Abort
			} 
			else startNextRun()
		}
		else startNextRun()
	}
	
	def startNextRun(){
		val seqWeighted = inBox.toSeq.map(_.value)
		val newTolerance = helpers.calculateNewTolerance(seqWeighted, currentTolerance / 2)
		log.info("New tolerance: {}", newTolerance)
		inBox = Set.empty
		currentWeightsTable = helpers.consolidateToWeightsTable(model)(seqWeighted)
		currentTolerance = newTolerance
		currentIteration = currentIteration + 1
		
		worker ! Job(seqWeighted, abcParams)
	}
}