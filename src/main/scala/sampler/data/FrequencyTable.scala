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
import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

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
	override def map[B](f: A => B) = new FrequencyTable[B]{
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

trait FrequencyTableBuilderComponent{
	val builder: FrequencyTableBuilder
}

trait FrequencyTableBuilder{
	def apply[T](samplable: Samplable[T])(condition: Seq[T] => Boolean)(implicit r: Random): FrequencyTable[T]
}

object SerialFrequencyTableBuilder extends FrequencyTableBuilder{
	def apply[T](samplable: Samplable[T])(condition: Seq[T] => Boolean)(implicit r: Random) = {
		@tailrec
		def takeMore(previous: Seq[T]): Seq[T] = {
			if(condition(previous)) previous
			else takeMore{
				(samplable.sample) +: previous 
			}
		}
		FrequencyTable(takeMore(Nil))
	}
}

class ParallelFrequencyTableBuilder(chunkSize: Int) extends FrequencyTableBuilder{
	def apply[T](samplable: Samplable[T])(condition: Seq[T] => Boolean)(implicit r: Random) = {
		@tailrec
		def takeMore(previous: ParSeq[T]): ParSeq[T] = {
			if(condition(previous.seq)) previous
			else takeMore{
				previous ++ ((1 to chunkSize).par.map(i => samplable.sample))
			}
		}
		val kickstart = (1 to chunkSize).par.map(i => samplable.sample)
		FrequencyTable(takeMore(kickstart).seq)
	}
}