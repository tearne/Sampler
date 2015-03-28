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

package sampler.abc.actor

import sampler.abc.Weighted
import sampler.abc.config.ABCConfig
import scala.language.existentials
import sampler.abc.Model
import sampler.abc.Scored
import akka.actor.ActorRef
import sampler.abc.Weighted
import sampler.abc.core.Generation
import sampler.data.Distribution
import sampler.math.Random
import sampler.abc.actor.algorithm.EvolvingGeneration
import sampler.data.DistributionBuilder

//TODO comment who sends what to whom

/**
 * Sent from [[sampler.cluster.]]
 */
case class Start[P](generationZero: Generation[P])
case class Report[P](generationId: Int, tolerance: Double, posterior: Seq[P])


//case class Finished()
case class ReportCompleted[P](report: Report[P])

sealed trait Job[P]
case class GenerateParticles[P](population: Map[P, Double], config: ABCConfig) extends Job[P]
object GenerateParticles{
	def buildFrom[P](gen: EvolvingGeneration[P], config: ABCConfig) = 
		GenerateParticles(
			gen.previousGen.particleWeights,
			config
		)
}

case class WeighJob[P](scored: ScoredParticles[P], previousWeights: Map[P, Double], tolerance: Double) extends Job[P]
object WeighJob{
	def buildFrom[P](gen: EvolvingGeneration[P]) = 
		WeighJob(
			gen.dueWeighing,
			gen.previousGen.particleWeights,
			gen.currentTolerance
		)
}

sealed trait WorkerResult[P]	// Things that are generated by workers

case class ScoredParticles[P](seq: Seq[Tagged[Scored[P]]]) extends WorkerResult[P]{
  def add(toAdd: ScoredParticles[P]) = ScoredParticles(seq ++ toAdd.seq)
  def add(toAdd: Seq[Tagged[Scored[P]]]) = ScoredParticles(seq ++ toAdd)
  
  def size = seq.length
}
object ScoredParticles{
	def empty[P] = ScoredParticles(Seq.empty[Tagged[Scored[P]]])
}

case class WeighedParticles[P](seq: Seq[Tagged[Weighted[P]]]) extends WorkerResult[P]{
  def add(toAdd: WeighedParticles[P]) = WeighedParticles(seq ++ toAdd.seq)
  def add(toAdd: Seq[Tagged[Weighted[P]]]) = WeighedParticles(seq ++ toAdd)
  
  def size = seq.length
}
object WeighedParticles{
	def empty[P] = WeighedParticles(Seq.empty[Tagged[Weighted[P]]])
}

case class MixPayload[P](tss: ScoredParticles[P])
case object Failed

case class Abort()
case class Aborted()
