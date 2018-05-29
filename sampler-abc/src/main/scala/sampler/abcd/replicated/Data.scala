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

import akka.cluster.ddata.DeltaReplicatedData
import sampler.abcd.Particle
import sampler.abcd.generation.{Generation, Population}
import sampler.abcd.replicated.delta.{DataDelta, DeltaItem}


case class Data[P](
    working: Population[P],
    previous: Generation[P],
    util: DataUtil = new DataUtil(),
    deltasOpt: Option[DataDelta[P]] = None
) extends DeltaReplicatedData {

  def addParticle(particle: Particle[P]): Data[P] = {
    copy(working = util.addParticle(working, particle))
      .addDelta(particle)
  }

  def logRejectedParticle(): Data[P] = ???

  def pushCompletedGeneration(completedGen: Population[P], nextTolerance: Double): Data[P] = {
    copy(
      working = util.nextEmptyPopulationBasedOn(completedGen, nextTolerance),
      previous = completedGen
    )
      .addDelta(completedGen)
  }

  private def addDelta(item: DeltaItem[P]): Data[P] = {
    copy(
      deltasOpt = deltasOpt match {
        case None => Option(DataDelta.newContaining(item))
        case some => some.map(_.addItem(item))
      }
    )
  }

  override type D = DataDelta[P]
  override type T = Data[P]

  override def delta: Option[D] = deltasOpt

  override def mergeDelta(thatDelta: D): T =
    util.merge(this, thatDelta) //TODO should returned value have empty deltas?

  override def resetDelta: T = copy(deltasOpt = None)

  override def merge(that: T): T = {
    assume(
      this.deltasOpt.isEmpty && that.deltasOpt.isEmpty,
      "Wrongly assumed no deltas present during merge"
    )

    //If this and that agree on 'previous' then merge 'working'
    if (this.working.iteration == that.working.iteration) {
      assume(previous == that.previous)
      Data(
        util.mergePopulations(this.working, that.working),
        previous
      )
    } else {
      //The one with the most recent generation number wins
      if (this.working precedes that.working)
        that
      else
        this
    }
  }

}
