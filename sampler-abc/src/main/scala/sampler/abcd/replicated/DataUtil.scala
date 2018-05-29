/*
 * Copyright (c) 2012-18 Crown Copyright
 *                       Animal and Plant Health Agency
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

package sampler.abcd.replicated

import sampler.abcd.Particle
import sampler.abcd.generation.Population
import sampler.abcd.replicated.delta.{DataDelta, DeltaItem, RejectedParticle}
import sampler.io.Logging

class DataUtil() extends Logging {
  def nextEmptyPopulationBasedOn[P](
      completedGen: Population[P],
      tolerance: Double
  ): Population[P] = Population[P](
    Seq.empty,
    0,
    completedGen.iteration + 1,
    tolerance
  )

  def merge[P](
      data: Data[P],
      delta: DataDelta[P]
  ): Data[P] = {
    if(delta.containsOnlyParticleDeltas){
      //There are only Particles and RejectedParticles (or empty seq), no Populations
      val currentIteration = data.working.iteration
      // Discard particles not for this generation
      val particleDeltas = delta
        .particleDeltaItems

      val newParticles = particleDeltas.collect{
        case p: Particle[P] if p.towardsGeneration == currentIteration => p
      }
      val numNewRejections = particleDeltas.collect{
        case p: RejectedParticle if p.towardsGeneration == currentIteration => p
      }.size

      val newWorking = data.working.copy(
        particles = newParticles ++: data.working.particles,
        numParticlesRejected = data.working.numParticlesRejected + numNewRejections
      )

      data.copy(working = newWorking)
    }



    ???
  }

  def mergePopulations[P](
      pop0: Population[P],
      pop1: Population[P]
  ): Population[P] = {
    assume(pop0.iteration == pop1.iteration)
    assume(pop0.tolerance == pop1.tolerance)
    assume(false, "what to do about acceptance ratio?")

    pop0.copy(particles =
        pop0.particles ++ pop1.particles
    )
  }

  def addParticle[P](
      pop: Population[P],
      particle: Particle[P]
  ): Population[P] = ???


//
//  def merge[P](
//      pop: Population[P],
//      delta: Seq[Weighted[P]]
//  ): Population[P] = {
//    pop.copy(
//        weightedParticles = pop.weightedParticles ++ delta
//    )
//  }
}
