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

class Empirical[A](val samples: IndexedSeq[A]) extends Samplable[A]{
	lazy val counts: Map[A, Int] = samples.groupBy(identity).mapValues(_.size)
	lazy val size = counts.values.sum
	lazy val relFreq = counts.mapValues(count => count.asInstanceOf[Double]/size)
	
	def apply(obs: A): Option[Int] = counts.get(obs)
	def sample(implicit r: Random): A = samples(r.nextInt(samples.size))
	def relativeFreq(obs: A): Option[Probability] = relFreq.get(obs).map(p => Probability(p))
	
	def map[B](f: A => B)(implicit r: Random) = new Empirical[B](samples.map(f))
	def flatMap[B](f: A => Empirical[B])(implicit r: Random) = new Empirical[B](
		samples.flatMap(s =>f(s).samples)
	)
	
	def +(item: A) = new Empirical[A](samples :+ item)
	def ++(items: TraversableOnce[A]) = new Empirical[A](samples ++ items)
	def +(that: Empirical[A]): Empirical[A] = new Empirical[A](samples ++ that.samples)
	
	def toDistribution(implicit order: Ordering[A]): Distribution[A] =
		throw new UnsupportedOperationException("TODO")
	
	def canEqual[A: Manifest](other: Any): Boolean = other.isInstanceOf[Empirical[_]]
	override def equals(other: Any) = other match {
		case that: Empirical[A] => 
			(that canEqual this) && (that.counts equals counts)
		case _ => false
	}
	
	override def hashCode() = counts.hashCode
}
object Empirical{
	def apply[T](s: Seq[T]) = new Empirical(s.toIndexedSeq)
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
	
	def max[T](a: Empirical[T], b: Empirical[T]): Double = {
		val indexes = a.counts.keySet ++ b.counts.keySet
		def distAtIndex(i: T) = math.abs(
				a.relativeFreq(i).map(_.value).getOrElse(0.0) -
				b.relativeFreq(i).map(_.value).getOrElse(0.0)
		)
		indexes.map(distAtIndex(_)).max
	}
}

object ParallelEmpiricalBuilder{
	def apply[T](samplable: Samplable[T], chunkSize: Int)(condition: ParSeq[T] => Boolean)(implicit r: Random): Empirical[T] = {
		@tailrec
		def takeMore(previous: ParSeq[T]): ParSeq[T] = {
			if(condition(previous)) previous
			else takeMore{
				previous ++ ((1 to chunkSize).par.map(i => samplable.sample))
			}
		}
		val kickstart = (1 to chunkSize).par.map(i => samplable.sample)
		Empirical(takeMore(kickstart).seq)
	}
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
