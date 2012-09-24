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
import scala.collection.Seq
import scala.collection.IndexedSeq
import scala.collection.TraversableOnce
import scala.math.Ordering
import scala.math.Fractional

/*
 * Specialisation of Samplable to apply to sets of observed data
 */
trait Empirical[Samp, Dom] extends Samplable[Samp]{ self => 
	val size: Int
	val probabilityMap: Map[Dom, Probability]
}

/*
 * Simple table of observations with uniform sampling
 */
trait FrequencyTable[A] extends Empirical[A, A]{ self =>
	val samples: IndexedSeq[A]
	
	lazy val size = counts.values.sum
	lazy val probabilityMap = counts.mapValues(count => Probability(count.asInstanceOf[Double]/size))
	def sample(implicit r: Random): A = samples(r.nextInt(samples.size))
	
	lazy val counts: Map[A, Int] = samples.groupBy(identity).mapValues(_.size)
	
	//Miles: Seems a shame to duplicate so much code here just to change return type
	override def filter(pred: A => Boolean): FrequencyTable[A] = new FrequencyTable[A]{
		val samples = self.samples.filter(pred)
	}
	
	//Miles: Would be nicer if the Monad bits (map and flatmap) could be in
	//       Samplable, but without needing to generalise the return type.
	//       Is this a place where the Monad in Scalaz could help?
	def map[B](f: A => B) = new FrequencyTable[B]{
		val samples = self.samples.map(f)
	}
	
	def flatMap[B](f: A => FrequencyTable[B])(implicit r: Random) = new FrequencyTable[B]{
		val samples = self.samples.flatMap(s =>f(s).samples)
	}

	def ++(items: TraversableOnce[A]) = new FrequencyTable[A]{
		val samples = self.samples ++ items
	}
	def +(that: FrequencyTable[A]): FrequencyTable[A] = new FrequencyTable[A]{
		val samples = self.samples ++ that.samples
	}
	
	def rightTail(itemInclusive: A)(implicit o: Ordering[A]): Probability = {
		val ordered = counts.keys.toList.sorted(o)	
		val value = ordered.dropWhile(i => o.lt(i,itemInclusive)).foldLeft(0.0){
			case (acc, i) => acc + probabilityMap(i).value
		}
		Probability(value)
	}
	
	//Miles: I tried to do this with a context bound instead of implicit arg 
	//       but failed Is it possible/desirable to use a context bound?
	def quantile(prob: Probability)(implicit f: Fractional[A]): A = {
		import f._
		val (lower, upper) = {
			val raw = prob.value * size - 1
			val idx = scala.math.ceil(raw).toInt
			if(idx <= 0) (0,0)
			else if(raw != math.floor(raw)) (idx, idx)
			else if(idx == size-1) (idx, idx)
			else (idx, idx + 1)
		}
		
		val ordered = samples.sorted
		val two = one + one
		
		(ordered(lower) + ordered(upper)) / two 
	}
	
	def canEqual[A: Manifest](other: Any): Boolean = other.isInstanceOf[FrequencyTable[_]]
	override def equals(other: Any) = other match {
		case that: FrequencyTable[_] => 
			(that canEqual this) && (that.counts equals counts)
		case _ => false
	}
	override def hashCode() = counts.hashCode
}

object FrequencyTable{
	def apply[T](seq: Seq[T]) = new FrequencyTable[T]{
		val samples = seq.toIndexedSeq
	}
}
object FrequencyTableBuilder{
	//Miles: Lots of nasty duplication in here, and the triple arg methods seems odd.  They are
	//       there to allow them to be called like this
	//FrequencyTableBuilder.serial(byDistribution){
	//    //Convergence condition
	//    _.size == 1e3
	//}
	//       Is there a better way of achieving a similar result?
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
}

//Miles: Was tempting to just put the mean (and other future statistics) directly in 
//       an object.  But we want to keep testing working in eclipse, and ScalaMock
//       needs SBT to mock objects.  So we put statistics in a class, but then  wrapped 
//       it in an object.  Problem is it involves a lots of duplication of the method 
//       signatures and seems overly wordy.  
class Statistic{
	def mean[T](emp: Empirical[_,T])(implicit num: Fractional[T]) = {
		import num._
		emp.probabilityMap.foldLeft(0.0){case (acc, (v,p)) => {
			acc + v.toDouble * p.value
		}} / emp.size	
	}
}
object Statistic{
	val instance = new Statistic
	def mean[T](emp: Empirical[_,T])(implicit num: Fractional[T]) = instance.mean(emp)
}


//Miles: As above with 'Statistic', I've tried to make a class with the implementation
//       for Distance, and then wrap it in an object.  But it seems very verbose.
class Distance(val stat: Statistic){
	def mean[T](a: Empirical[_,T], b: Empirical[_,T])(implicit num: Fractional[T]) = {
		math.abs(stat.mean(a)-stat.mean(b))
	}
	
	def max[T](a: Empirical[_,T], b: Empirical[_,T]): Double = {
		val indexes = a.probabilityMap.keySet ++ b.probabilityMap.keySet
		def distAtIndex(i: T) = math.abs(
				a.probabilityMap.get(i).map(_.value).getOrElse(0.0) -
				b.probabilityMap.get(i).map(_.value).getOrElse(0.0)
		)
		indexes.map(distAtIndex(_)).max
	}
}

object Distance{
	val instance = new Distance(new Statistic)
	def mean[T](a: Empirical[_,T], b: Empirical[_,T])(implicit num: Fractional[T]) = 
		instance.mean(a,b)(num)
	
	def max[T](a: Empirical[_,T], b: Empirical[_,T]): Double = 
		instance.max(a,b)
}
