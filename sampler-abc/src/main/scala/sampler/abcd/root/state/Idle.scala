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
import sampler.abcd.generation.Generation
import sampler.abcd.root.task.Task
import sampler.abcd.root.{Dependencies, Start}

case class Idle[P](dependencies: Dependencies[P]) extends State[P] {
  import dependencies._

  override def evolve(sender: ActorRef, self: ActorRef): PartialFunction[Any, State[P]] = {

    case startMsg: Start =>
      val task: Task[P] = util.kickOff(startMsg, childRefs, sender)
      //TODO report that a generation is being started
      // utils.sendStatusReport(...)
      Generating(dependencies, task)

    case other => reportAndIgnoreUnexpected(other)
  }
}
