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

package sampler.abcd.root

import akka.actor.ActorRef
import akka.cluster.ddata.Key
import sampler.abcd.generation.Generation
import sampler.abcd.replicated.{PrevGenData, WorkingGenData}

case class Utils() {

  def initialise[P](startMsg: Start, prevGen: Generation[P], childRefs: ChildRefs, sender: ActorRef) = ???

  def shouldFlush[P](workingGenData: Key[WorkingGenData[P]] => WorkingGenData[P], config: Any): Boolean = ???

  def startFlush[P](workingGenData: Key[WorkingGenData[P]] => WorkingGenData[P]) = ???

  def requestParticle[P](freeWorker: ActorRef, prevGenData: PrevGenData[P]) = ???
}
