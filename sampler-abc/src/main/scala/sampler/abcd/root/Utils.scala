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
import sampler.abcd.ABCConfig
import sampler.abcd.generation.Generation
import sampler.abcd.replicated.{PrevGenData, WorkingGenData}
import sampler.abcd.root.task.Task

case class Utils() {

  def initialise[P](startMsg: Start, prevGen: Generation[P], childRefs: ChildRefs, sender: ActorRef): Unit = ???

  def shouldFlush[P](workingGenData: WorkingGenData[P], task: Task[P]): Boolean = ???

  def startFlush[P](freeWorker: ActorRef, workingGenData: WorkingGenData[P]): Unit = ???

  def requestParticle[P](freeWorker: ActorRef, prevGenData: PrevGenData[P]): Unit = ???

  def startNewGeneration[P](prevGen: Generation[P], childRefs: ChildRefs): Unit = ???

  def shouldTerminate[P](prevGen: Generation[P], task: Task[P]): Boolean = ???

  def startTermination[P](): Unit = ???

  def sendResultToClient[P](task: Task[P], result: PrevGenData[P]): Unit = ???

}
