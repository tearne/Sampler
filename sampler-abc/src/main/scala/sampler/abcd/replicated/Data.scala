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

import java.util.UUID

import akka.cluster.ddata.ReplicatedData
import sampler.abcd.Particle
import sampler.abcd.generation.{Generation, Population}

case class Data[P](
    population: Population[P],
    ancestorUUID: Option[UUID],
    util: DataUtil = new DataUtil()
) extends ReplicatedData {

  def addParticle(particle: Particle[P]): Data[P] = {
    copy(working = util.addParticle(working, particle))
  }

  def logRejectedParticle(): Data[P] = ???

  def pushCompletedGeneration(completedGen: Population[P], nextTolerance: Double): Data[P] = {
    copy(
      working = util.nextEmptyPopulationBasedOn(completedGen, nextTolerance),
      previous = completedGen
    )
  }

  override type T = Data[P]

  override def merge(that: T): T = {
    //If this and that agree on 'previous' then merge 'working'
    if (this.previous == that.previous) {
      Data(
        util.mergeWorkingPopulations(this.working, that.working),
        previous
      )
    } else {
      /*
      If we have two different version of the previous generation then we can't
      merge the working generations, as they would have been produced from
      different proposal distributions.  We have to choose a winner
       */
      if (this.previous winsOver that.previous)
        that
      else
        this
    }
  }

}
