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
import scala.annotation.tailrec
import scala.collection.GenSeq

trait Samplable[A]{ self =>
	//NOTE: Passing the Random explicitly as arg since we don't necessarily 
	// want to have the random sent to other remote nodes during distributed
	// computation.  This was we have options.
	def sample(implicit r: Random): A
	
	def sampleUntil(condition: GenSeq[A] => Boolean, r: Random): GenSeq[A] = {
		@tailrec
		def prepend(previous: GenSeq[A]): GenSeq[A] = {
			if(condition(previous)) previous
			else prepend(previous.+:(sample(r)))
		}
		prepend(Nil)
	}
	
	def map[B](f: A => B)(implicit r: Random) = new Samplable[B]{
		override def sample(implicit r: Random) = f(self.sample(r))
	}
	
	def flatMap[B](f: A => Samplable[B])(implicit r: Random) = new Samplable[B]{
		override def sample(implicit r: Random) = f(self.sample(r)).sample(r)
	}
	
	def filer(predicate: A => Boolean)(implicit r: Random) = new Samplable[A]{
		@tailrec
		override def sample(implicit r: Random): A = {
			val s = self.sample(r)
			if(predicate(s)) s
			else sample(r)
		}
	}
}

object Samplable{
	//Basic building blocks
	def uniform(lower: Double, upper: Double)(implicit r: Random) = new Samplable[Double]{
		def sample(implicit r: Random) = (upper - lower) * r.nextDouble
	}
	def uniform[T](items: IndexedSeq[T])(implicit r: Random) = new Samplable[T]{
		val size = items.size
		def sample(implicit r: Random) = items(r.nextInt(size))
	}
	def binaryPopulation(numInfected: Int, size: Int)(implicit r: Random) = new Samplable[Boolean]{
		def sample(implicit r: Random) = r.nextInt(size) < numInfected
	}
}

class Empirical[T](val samples: IndexedSeq[T]) extends Samplable[T]{
	lazy val counts: Map[T, Int] = samples.groupBy(identity).mapValues(_.size)
	lazy val size = counts.values.sum
	lazy val relFreq = counts.mapValues(count => count.asInstanceOf[Double]/size)
	
	def +(item: T) = new Empirical(this.samples :+ item)
	def ++(items: TraversableOnce[T]) = new Empirical(this.samples ++ items)
	def +(that: Empirical[T]): Empirical[T] = new Empirical(this.samples ++ that.samples)
	
	def apply(obs: T): Option[Int] = counts.get(obs)
	def relativeFreq(obs: T): Option[Probability] = relFreq.get(obs).map(p => Probability(p))
	def sample(implicit r: Random): T = samples(r.nextInt(samples.size))

	def toDistribution(implicit order: Ordering[T]): Distribution[T] =
		throw new UnsupportedOperationException("TODO")
	def canEqual[T: Manifest](other: Any): Boolean = other.isInstanceOf[Empirical[_]]
	override def equals(other: Any) = other match {
		case that: Empirical[T] => 
			(that canEqual this) && (that.counts equals counts)
		case _ => false
	}
	
	override def hashCode() = counts.hashCode
}

trait Distribution[T] extends Empirical[T]{
	def cdf(elem: T): Double 
	def quantile(p: Probability): T
}

class Distance(val stat: Statistic){
	def mean[T](a: Empirical[T], b: Empirical[T])(implicit num: Fractional[T]) = {
		import num._
		abs(stat.mean(a)-stat.mean(b))
	}
}
object Distance{
	val instance = new Distance(new Statistic)
	def mean[T](a: Empirical[T], b: Empirical[T])(implicit num: Fractional[T]) = 
		instance.mean(a,b)(num)
}

class Statistic{
	def mean[T](emp: Empirical[T])(implicit num: Fractional[T]) = {
		import num._
		emp.counts.foldLeft(num.zero){case (acc, (v,c)) => {
			acc + v * num.fromInt(c)
		}} / num.fromInt(emp.size)	
	}
}
object Statistic{
	val instance = new Statistic
	def mean[T](emp: Empirical[T])(implicit num: Fractional[T]) = instance.mean(emp)
}