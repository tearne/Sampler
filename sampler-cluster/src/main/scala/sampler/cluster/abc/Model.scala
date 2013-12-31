/*
 * Copyright (c) 2012 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
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

package sampler.cluster.abc

import sampler.math.{ Random, StatisticsComponent }
import sampler.data.Distribution

case class Scored[+A](params: A, repScores: Seq[Double]){
	def numReps = repScores.size.toDouble
}
case class Weighted[+A](scored: Scored[A], weight: Double){
	def params = scored.params
	def repScores = scored.repScores
	def meanRepScore = repScores.sum.toDouble / repScores.size
}

trait Prior[A] extends Distribution[A]{
	def density(value: A): Double
}

trait Model[P] {
	val prior: Prior[P]
	def perturb(parameters: P): P
	def perturbDensity(a: P, b: P): Double	
	def distanceToObservations(p: P): Distribution[Double]
}