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
import sampler.abcd.root.Dependencies
import sampler.abcd.root.task.Task


trait State[P]{
  def evolve(sender: ActorRef, self: ActorRef): PartialFunction[Any, State[P]]
  def dependencies: Dependencies[P]

  val ignore = this
  val stay = this

  def reportAndIgnoreUnexpected(msg: Any) = {
    dependencies.log.warning(
      "Unexpected message encountered in {}: {} [...]",
      getClass,
      msg.toString.take(50)
    )
    this
  }
}