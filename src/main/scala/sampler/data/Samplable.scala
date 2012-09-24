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
import scala.annotation.tailrec
import org.apache.commons.math3.distribution.NormalDistribution

/*
 * Anything from which we can draw samples.  E.g. an analytical distribution,
 * or bootstrapping from a data set of observations
 */
trait Samplable[A]{ self =>
	def sample(implicit r: Random): A

	def until(condition: IndexedSeq[A] => Boolean)(implicit r: Random) = new Samplable[IndexedSeq[A]]{
		def sample(implicit r: Random): IndexedSeq[A] = {
			@tailrec
			def append(previous: IndexedSeq[A]): IndexedSeq[A] = {
				if(condition(previous)) previous
				else append(previous.:+(self.sample))
			}
			append(IndexedSeq[A](self.sample))
		}
	}
	def filter(predicate: A => Boolean) = new Samplable[A]{
		@tailrec
		override def sample(implicit r: Random): A = {
			val s = self.sample(r)
			if(predicate(s)) s
			else sample(r)
		}
	}
	
	//TODO when sub-traits override this the return type gets messed up
	def map[B](f: A => B)(implicit r: Random) = new Samplable[B]{
		def sample(implicit r: Random) = f(self.sample(r))
	}
	
	def flatMap[B](f: A => Samplable[B])(implicit r: Random) = new Samplable[B]{
		def sample(implicit r: Random) = f(self.sample(r)).sample(r)
	}
	
	def combine[B](that: Samplable[A], op: Function2[A,A,B]) = new Samplable[B]{
		def sample(implicit r: Random) = op(self.sample(r), that.sample(r))
	}
	
	def +(that: Samplable[A])(implicit n: Numeric[A]) = combine(that, n.plus _)
	def -(that: Samplable[A])(implicit n: Numeric[A]) = combine(that, n.minus _)
	
	
}

object Samplable{
	def diracDelta[T](value: T) = new Samplable[T]{
		def sample(implicit r: Random) = value
	}
	
	def uniform(lower: Double, upper: Double)(implicit r: Random) = new Samplable[Double]{
		def sample(implicit r: Random) = (upper - lower) * r.nextDouble()
	}
	
	def uniform[T](items: IndexedSeq[T])(implicit r: Random) = new Samplable[T]{
		val size = items.size
		def sample(implicit r: Random) = items(r.nextInt(size))
	}
	
	def withoutReplacement[T](items: IndexedSeq[T], sampleSize: Int) = new Samplable[List[T]]{
		def sample(implicit r: Random) = {
			@tailrec
			def takeAnother(acc: List[T], bag: IndexedSeq[T]): List[T] = {
				if(acc.size == sampleSize) acc
				else{ 
					val item = bag(r.nextInt(bag.size))
					takeAnother(item +: acc, bag diff List(item))
				}
			}
			
			takeAnother(Nil, items)
		}
	}
	
	def binaryPopulation(numInfected: Int, size: Int)(implicit r: Random) = new Samplable[Boolean]{
		def sample(implicit r: Random) = r.nextInt(size) < numInfected
	}
	
	def normal(mean:Double, variance: Double) = new Samplable[Double]{
		val d = new NormalDistribution(0,variance)
		def sample(implicit r: Random) = d.sample
		def density(value: Double) = d.density(value)
	}
}
