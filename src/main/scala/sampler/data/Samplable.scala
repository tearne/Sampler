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

package sampler.data

import sampler.math.Probability
import sampler.math.Random

trait Samplable[T]{
	def sample(implicit rand: Random): T
}

trait Empirical[T] extends Samplable[T]{
	def +(item: T): Unit
	def ++(items: TraversableOnce[T]): Unit
	def +(that: Empirical[T]): Empirical[T]
	
	def apply(obs: T): Int
	def relativeFreq(obs: T): Probability
	def counts(): Map[T, Int]

	def filter(predicate: T => Boolean): Empirical[T]
	
	def toDistribution(implicit order: Ordering[T]): Distribution[T]
	
	def map[B](f: T => B): Empirical[B]
	def flatMap[B](f: T => Empirical[B]): Empirical[B]
}

trait Distribution[T] extends Empirical[T]{
	def cdf(elem: T): Double 
	def quantile(p: Probability): T
}

trait EmpiricalMetric[T]{
	def distance(a: Empirical[T], b: Empirical[T])
}