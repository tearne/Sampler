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

import akka.cluster.ddata.ReplicatedData
import sampler.abcd.Particle

case class WorkingGenData[P](
    particles: Set[Particle[P]],
    generationId: Int
) extends ReplicatedData {
  //TODO Deltas using ORSet

  /**
    * Note that we don't guarantee that the particles added
    * came from the same previous generation
    */
  def addParticle(p: Particle[P]): WorkingGenData[P] = {
    if(p.towardsGeneration == generationId)
      copy(particles = particles + p)
    else if(p.towardsGeneration > generationId)
      copy(particles = Set(p), generationId = p.towardsGeneration)
    else {
      //TODO log me: particle for old generation arrived
      this
    }
  }

  /**
    * Since the particles may have come from different previous
    * generations (prior to CRDT convergence) we need to count
    * in groups based on parent generation
    */
  def numConsistentParticles(): Int = particles
    .groupBy(_.parentGenerationUUID)
    .values
    .map(_.size)
    .max

  override type T = WorkingGenData[P]

  override def merge(that: T): T = {
    if(this.generationId == that.generationId)
      copy(that.particles ++ particles)
    else if(this.generationId > that.generationId)
      this
    else
      that
  }
}
object WorkingGenData{
  def empty[P]: WorkingGenData[P] = WorkingGenData(Set.empty[Particle[P]], 0)
}