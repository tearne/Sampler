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

import sampler.math._

case class Particle[A](value: A, weight: Double)

/*
 * A table of observations/values where each is attached to a weight.  
 * Sampling is performed according to the weights
 */
trait WeightsTable[A] extends Empirical[Particle[A], A]{ self =>
	val particles: Seq[Particle[A]]
	
	lazy val size = particles.size
	
	//Miles: Something feels odd here.  Since there is no constructor, the incoming particles seq is
	//       transformed by normalising the weights, but then instead of just keeping the normalised
	//       particles, we are stuck with holding on to the original particles sequence too.
	lazy val normalised: IndexedSeq[Particle[A]] = {
		val total = particles.map(_.weight).sum
		val normalised = particles.map(particle => Particle(particle.value, particle.weight / total))

		val sum = normalised.map(_.weight).sum
		//TODO clean up this check and exception
		if(math.abs(sum - 1.0)>0.0000001)
			throw new RuntimeException("Sum not 1: "+sum)
		normalised.toIndexedSeq
	}	
	lazy val cumulativeWeights = normalised.scanLeft(0.0){case (acc, particle) => acc + particle.weight}.tail
	lazy val probabilityMap = normalised.map(p => (p.value, Probability(p.weight))).toMap
	lazy val values = probabilityMap.values
	//Miles: Seems odd that I've had to make so many vals above lazy. Some of them are to avoid 
	//       null pointers when instantiating with new WeightsTable[T]{ ... }, others are to
	//       avoid unnecessary processing at construction time.
	
	def sample(implicit r: Random): Particle[A] = {
		//TODO Use the alias method
		val rnd = r.nextDouble()
		val index = cumulativeWeights.zipWithIndex.find(_._1 > rnd) match {
			case None => {
				println(cumulativeWeights)
				println(normalised)
				cumulativeWeights.size
			}
			case Some(tuple) => tuple._2
		}
		normalised(index)
	}
	
	def discardWeights(): FrequencyTable[A] = new FrequencyTable[A]{
		val samples = particles.map(_.value).toIndexedSeq
	}
	
	//Miles: Again, this is feeling like an unnecessary amount of duplication for a
	//       return type change
	override def filter(pred: Particle[A] => Boolean) = new WeightsTable[A]{
		val particles = self.particles.filter(pred)
	}

	//Miles: As with FrequencyTable, it seems odd that the monad stuff isn't in the Samplable trait
	def map[B](f: Particle[A] => Particle[B]): WeightsTable[B] = new WeightsTable[B]{
		val particles = self.particles.map{p => f(p)}
	}
	def flatMap[B](f: Particle[A] => WeightsTable[B])(implicit r: Random) = new WeightsTable[B]{
		val particles = self.particles.flatMap(p => f(p).particles)
	}
	
	def canEqual[A: Manifest](other: Any): Boolean = other.isInstanceOf[WeightsTable[_]]
	override def equals(other: Any) = other match {
		case that: WeightsTable[_] => 
			(that canEqual this) && (that.probabilityMap == probabilityMap)
		case _ => false
	}
	override def hashCode() = probabilityMap.hashCode
}

object WeightsTable{
	def apply[T](p: Seq[Particle[T]]): WeightsTable[T] = new WeightsTable[T]{
		val particles: Seq[Particle[T]] = p
	}
	
	def buildFrom[T](condition: Seq[T] => Boolean)(implicit r: Random) = {
		
	}
}