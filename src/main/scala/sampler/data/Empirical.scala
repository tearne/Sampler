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

import sampler.math.Random
import sampler.math.Probability
import scala.annotation.tailrec
import scala.collection.parallel.ParSeq
import scala.collection.GenSeq

trait Empirical[A] extends Samplable[A]{
	val size: Int
	val probabilityMap: Map[A, Probability]
	def sample(implicit r: Random): A
}

class FrequencyTable[A](val samples: IndexedSeq[A]) extends Empirical[A]{
	lazy val counts: Map[A, Int] = samples.groupBy(identity).mapValues(_.size)

	lazy val size = counts.values.sum
	def sample(implicit r: Random): A = samples(r.nextInt(samples.size))
	val probabilityMap = counts.mapValues(count => Probability(count.asInstanceOf[Double]/size))
	
	def map[B](f: A => B) = new FrequencyTable[B](samples.map(f))
	def flatMap[B](f: A => FrequencyTable[B])(implicit r: Random) = new FrequencyTable[B](
		samples.flatMap(s =>f(s).samples)
	)
	
	def +(item: A) = new FrequencyTable[A](samples :+ item)
	def ++(items: TraversableOnce[A]) = new FrequencyTable[A](samples ++ items)
	def +(that: FrequencyTable[A]): FrequencyTable[A] = new FrequencyTable[A](samples ++ that.samples)
	
	def canEqual[A: Manifest](other: Any): Boolean = other.isInstanceOf[FrequencyTable[_]]
	override def equals(other: Any) = other match {
		case that: FrequencyTable[A] => 
			(that canEqual this) && (that.counts equals counts)
		case _ => false
	}
	override def hashCode() = counts.hashCode
}
object FrequencyTable{
	def apply[T](seq: Seq[T]) = new FrequencyTable(seq.toIndexedSeq)
}
object FrequencyTableBuilder{
	def serial[T](samplable: Samplable[T])(condition: Seq[T] => Boolean)(implicit r: Random): FrequencyTable[T] ={
		@tailrec
		def takeMore(previous: Seq[T]): Seq[T] = {
			if(condition(previous)) previous
			else takeMore{
				(samplable.sample) +: previous 
			}
		}
		FrequencyTable(takeMore(Nil))
	}
	
	def parallel[T](samplable: Samplable[T], chunkSize: Int)(condition: ParSeq[T] => Boolean)(implicit r: Random): FrequencyTable[T] = {
		@tailrec
		def takeMore(previous: ParSeq[T]): ParSeq[T] = {
			if(condition(previous)) previous
			else takeMore{
				previous ++ ((1 to chunkSize).par.map(i => samplable.sample))
			}
		}
		val kickstart = (1 to chunkSize).par.map(i => samplable.sample)
		FrequencyTable(takeMore(kickstart).seq)
	}
}

case class Particle[A](value: A, weight: Double)
class WeightsTable[A](val particles: Seq[Particle[A]]) extends Empirical[A]{
	val normalised: IndexedSeq[Particle[A]] = {
		val total = particles.map(_.weight).sum
		val normalised = particles.map(particle => Particle(particle.value, particle.weight / total))
		//Check weights
		//TODO clean up
		val sum = normalised.map(_.weight).sum
		if(math.abs(sum - 1.0)>0.0000001)
			throw new RuntimeException("Sum not 1: "+sum)
		normalised.toIndexedSeq
	}	
	lazy val cumulativeWeights = normalised.scanLeft(0.0){case (acc, particle) => acc + particle.weight}
	
	val size = particles.size
	val probabilityMap = normalised.map(p => (p.value, Probability(p.weight))).toMap
	def sample(implicit r: Random): A = {
		val rnd = r.nextDouble()
		val index = cumulativeWeights.zipWithIndex.find(_._1 > rnd) match {
			case None => cumulativeWeights.size		
			case Some(tuple) => tuple._2 - 1
		}
		normalised(index).value
	}
	
	def map[B](f: Particle[A] => Particle[B]): WeightsTable[B] = {
		new WeightsTable(particles.map{p => f(p)})
	}
	def mapValues[B](f: A => B): Seq[B] = {
		particles.map(p => f(p.value))
	}
	//TODO flatMap
	
	
	lazy val weightsMap = normalised.map(p => (p.value, p.weight)).toMap
	def canEqual[A: Manifest](other: Any): Boolean = other.isInstanceOf[WeightsTable[_]]
	override def equals(other: Any) = other match {
		case that: WeightsTable[A] => 
			(that canEqual this) && (that.weightsMap == weightsMap)
		case _ => false
	}
	override def hashCode() = weightsMap.hashCode
}

class Distance(val stat: Statistic){
	def mean[T](a: Empirical[T], b: Empirical[T])(implicit num: Fractional[T]) = {
		math.abs(stat.mean(a)-stat.mean(b))
	}
}

object Distance{
	val instance = new Distance(new Statistic)
	def mean[T](a: Empirical[T], b: Empirical[T])(implicit num: Fractional[T]) = 
		instance.mean(a,b)(num)
	
	def max[T](a: Empirical[T], b: Empirical[T]): Double = {
		val indexes = a.probabilityMap.keySet ++ b.probabilityMap.keySet
		def distAtIndex(i: T) = math.abs(
				a.probabilityMap.get(i).map(_.value).getOrElse(0.0) -
				b.probabilityMap.get(i).map(_.value).getOrElse(0.0)
		)
		indexes.map(distAtIndex(_)).max
	}
}

class Statistic{
	def mean[T](emp: Empirical[T])(implicit num: Fractional[T]) = {
		import num._
		emp.probabilityMap.foldLeft(0.0){case (acc, (v,p)) => {
			acc + v.toDouble * p.value
		}} / emp.size	
	}
}
object Statistic{
	val instance = new Statistic
	def mean[T](emp: Empirical[T])(implicit num: Fractional[T]) = instance.mean(emp)
}
