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

package sampler.abcd.replicated

import akka.cluster.ddata.ReplicatedData
import sampler.abcd.generation.{Generation, Population}

case class PrevGenData[P](
    generation: Generation[P]
) extends ReplicatedData {

  def replaceWith(newPop: Population[P]): PrevGenData[P] = {
    if(newPop.iteration > this.generation.iteration)
      PrevGenData(newPop)
    else
      this //TODO log me?
  }

  override type T = PrevGenData[P]

  override def merge(that: T): T = {
    if(this == that)
      this
    else if(this.generation.iteration > that.generation.iteration)
      this
    else if(this.generation.iteration < that.generation.iteration)
      that
    else if(this.generation.uuid.get.timestamp < that.generation.uuid.get.timestamp)  //TODO better option handling
      this
    else
      that
  }
}
object PrevGenData{
  def empty[P](startGen: Generation[P]) = PrevGenData(startGen)
}
