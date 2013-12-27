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
import sampler.cluster.abc.actor.root.EncapsulatedState
import sampler.cluster.abc.actor.root.Tagged
import sampler.cluster.abc.parameters.ABCParameters
import scala.language.existentials

case class Start(eState: EncapsulatedState)
case class Result(params: Seq[ABCModel#ParameterSet])

case class Job(population: Map[_, Double], abcParams: ABCParameters)

case class TaggedAndScoredParameterSets[T](seq: Seq[Tagged[T]])

case class Abort()
case class Aborted()
