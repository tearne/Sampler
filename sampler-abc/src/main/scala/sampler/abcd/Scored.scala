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

case class Scored[P](params: P, scores: Seq[Double], id: Option[UUID]){
  def meanScore: Double = scores.sum / scores.size
  def wasLocallyGenerated: Boolean = id.exists(_.generatingNodeId == UUID.thisNodeId)
}

object Scored{
  def apply[P](params: P, scores: Seq[Double]): Scored[P] =
    Scored(
      params,
      scores,
      Some(UUID.generate())
    )
}