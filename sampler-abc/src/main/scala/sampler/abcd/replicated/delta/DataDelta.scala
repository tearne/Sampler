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

package sampler.abcd.replicated.delta

import sampler.abcd.generation.Population

case class DataDelta[P](items: Seq[DeltaItem[P]]){
  def addItem(item: DeltaItem[P]) = copy(items = item +: items)

  lazy val particleDeltaItems = items.collect{
    case p: ParticleDeltaItem[P] => p
  }

  def containsOnlyParticleDeltas(): Boolean = {
    particleDeltaItems.size == items.size
  }
}

object DataDelta {
  def newContaining[P](item: DeltaItem[P]) = DataDelta(Seq(item))
}
