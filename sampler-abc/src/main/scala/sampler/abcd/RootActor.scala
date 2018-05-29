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

package sampler.abcd

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.cluster.ddata.DistributedData
import sampler.abcd.root.state.{Idle, State}
import sampler.abcd.root.{ChildRefs, Dependencies, Utils}

class RootActor[P](
    utils: Utils,
    childRefs: ChildRefs
) extends Actor with ActorLogging {

  val replicator: ActorRef = DistributedData(context.system).replicator

  override def receive: Receive = behaviour(
    Idle(Dependencies(utils, childRefs, context, log))
  )

//
//  {
//    case NewParticle(p: Particle[P]) =>
//
//
//    {
//      if(replicator.)
//
//
//        replicator ! Update(WorkingGenKey, GenerationData.empty(), WriteMajority)(_.addParticle(p))
//      // TODO If generation data is not empty, and p was generated from a different proposal generation...
//      // ... choose
//
//      // If got enough, abort generationa and flush
//      ???
//    }
//    case NewPopulation(pop: Population[P]) => ???
//  }

  def behaviour(state: State[P]): Receive = {
    case message => context.become(
      behaviour(state.evolve(sender, self)(message))
    )
  }

}
