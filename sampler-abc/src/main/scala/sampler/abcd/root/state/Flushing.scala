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

package sampler.abcd.root.state

import akka.actor.ActorRef
import akka.cluster.ddata.Replicator._
import sampler.abc.actor.root.state.Gathering
import sampler.abcd.generation.Population
import sampler.abcd.replicated.PrevGenData
import sampler.abcd.root.{Dependencies, FlushComplete, NewParticle}
import sampler.abcd.root.task.Task

case class Flushing[P](dependencies: Dependencies[P],task: Task[P]) extends State[P] {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, State[P]] = {
    case _: NewParticle[P] => ignore

    case fc: FlushComplete[P] =>
      // TODO Update PrevGenData with flushedGen
      val flushedPop = fc.population
      replicator ! Update(PrevGenKey, WriteLocal, None){case data: PrevGenData[P] =>
        data.replaceWith(flushedPop)}
      stay

    case UpdateSuccess(PrevGenKey, None) =>
      // Now the gen has been flushed, do a get to ensure we see the outcome of any background updates/merges
      replicator ! Get(PrevGenKey, ReadLocal, None )
      stay

    case gs @ GetSuccess(PrevGenKey, None) =>
      val prevGen = gs.get(PrevGenKey).generation
      //Determine if should quit
      if (util.shouldTerminate(prevGen, task)) {
        util.startTermination()
        Terminating(dependencies, task)
      } else {
        util.startNewGeneration(prevGen, childRefs)
        Generating(dependencies, task)
      }

    case other => reportAndIgnoreUnexpected(other)
  }
}
