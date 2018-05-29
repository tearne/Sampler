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
import sampler.abcd.replicated.WorkingGenData
import sampler.abcd.root.{Dependencies, NewParticle, Start}
import sampler.abcd.root.task.Task


case class Generating[P](dependencies: Dependencies[P], task: Task[P]) extends State[P] {

  import dependencies._

  override def evolve(sender: ActorRef, self: ActorRef): PartialFunction[Any, State[P]] = {
    case np: NewParticle[P] =>
      // Now we've got a new particle, let's also get an update to the previous generation in case anything changed
      val freeWorkerNode = sender
      replicator ! Update(WorkingGenKey, WorkingGenData.empty, WriteLocal, Some(freeWorkerNode))(_ addNewParticle p)
      stay


      //TODO DANGER, this flow will go screwey with more than two workers, since
      // the success messages for each might get confused

    case us @ UpdateSuccess(WorkingGenKey, Some(freeWorker: ActorRef)) =>
      replicator ! Get(WorkingGenKey, ReadLocal, Some(freeWorker))
      stay

    case gs @ GetSuccess(WorkingGenKey, Some(freeWorker: ActorRef)) =>
      val workingPopulation = gs.get[WorkingGenData[P]].population
      if(util.hasEnoughParticles(workingPopulation, config)) {
        util.flush(workingGenData)
        Flushing()
      }
      else {
        replicator ! Get(PrevGenKey, ReadLocal, Some(freeWorker))
        stay
      }

    case gs @ GetSuccess(PrevGenKey, Some(frereWorker: ActorRef)) =>
      val prevPopulation = gs.get[PrevGenData].generation
      util.allocateWork(freeWorker, prevPopulation)
      stay
    ???
  }
}
