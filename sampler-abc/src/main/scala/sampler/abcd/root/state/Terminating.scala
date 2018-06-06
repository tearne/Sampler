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
import akka.cluster.ddata.Replicator.{Get, GetSuccess, ReadLocal}
import sampler.abcd.root.{Dependencies, ReportCompleted}
import sampler.abcd.root.task.Task

case class Terminating[P](
    dependencies: Dependencies[P],
    task: Task[P]
  ) extends State[P] {

  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, State[P]] =
  {
    case ReportCompleted =>
      replicator ! Get(PrevGenKey, ReadLocal, None)
      stay

    case gs @ GetSuccess(PrevGenKey, None) =>
      val result = gs.get(PrevGenKey)
      util.sendResultToClient(task, result)
      stay

    case other => reportAndIgnoreUnexpected(other)
  }
}
