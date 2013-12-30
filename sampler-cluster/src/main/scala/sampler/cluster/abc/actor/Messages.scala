/*
 * Copyright (c) 2012-13 Crown Copyright 
 *                       Animal Health and Veterinary Laboratories Agency
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

package sampler.cluster.abc.actor

import sampler.abc.ABCModel
import sampler.abc.Weighted
import sampler.cluster.abc.parameters.ABCParameters
import scala.language.existentials
import sampler.cluster.abc.state.State
import sampler.cluster.abc.Model
import sampler.cluster.abc.Scored

case class Start[P](state: State[P])
case class Result[P](params: Seq[P])

case class Job[P](population: Map[P, Double], abcParams: ABCParameters)

case class TaggedScoreSeq[P](seq: Seq[Tagged[Scored[P]]])

case class Abort()
case class Aborted()
