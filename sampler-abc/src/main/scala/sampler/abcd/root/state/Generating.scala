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
import sampler.abcd.replicated.{PrevGenData, WorkingGenData}
import sampler.abcd.root.task.Task
import sampler.abcd.root.{Dependencies, NewParticle}


case class Generating[P](dependencies: Dependencies[P], task: Task[P]) extends State[P] {

  import dependencies._

  override def evolve(sender: ActorRef, self: ActorRef): PartialFunction[Any, State[P]] = {
    case NewParticle(p) =>
      val freeWorkerNode = sender
      replicator ! Update(WorkingGenKey, WorkingGenData.empty, WriteLocal, Some(freeWorkerNode))(_ addParticle p)
      stay

      //TODO, think about whether there is a problem if lots of new particles com in at once

    case UpdateSuccess(WorkingGenKey, Some(freeWorker: ActorRef)) =>
      // Adding the new particle is done, now get the whole thing to see how many particles we have
      replicator ! Get(WorkingGenKey, ReadLocal, Some(freeWorker))
      stay

    case gs @ GetSuccess(WorkingGenKey, Some(freeWorker: ActorRef)) =>
      val workingGenData: WorkingGenData[P] = gs.get(WorkingGenKey)
      if(util.shouldFlush(workingGenData, config)) {
        util.startFlush(workingGenData) //This should abort workers, send flush job, etc
        Flushing(dependencies, task) //TODO should we update the task in some way?
      }
      else {
        /**
        Not time to flush.
        Get the latest prev gen (in case it changed from another node having flushed)
        and tell worker to build another particle
         **/
        replicator ! Get(PrevGenKey, ReadLocal, Some(freeWorker))
        stay
      }

    case gs @ GetSuccess(PrevGenKey, Some(freeWorker: ActorRef)) =>
      val prevGenData = gs.get(PrevGenKey)
      util.requestParticle(freeWorker, prevGenData)
      stay
    ???
  }
}
