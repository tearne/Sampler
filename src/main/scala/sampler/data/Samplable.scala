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

trait Samplable[A]{ self =>
	//NOTE: Passing the Random explicitly as arg since we don't necessarily 
	// want to have the random sent to other remote nodes during distributed
	// computation.  This was we have options.
	def sample(implicit r: Random): A
	
	def buildEmpirical(condition: Seq[A] => Boolean)(implicit r: Random): Empirical[A] = {
		@tailrec
		def prepend(previous: Seq[A]): Seq[A] = {
			if(condition(previous)) previous
			else prepend(previous.+:(sample(r)))
		}
		Empirical(prepend(sample(r) :: Nil))
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
	def uniform(lower: Double, upper: Double)(implicit r: Random) = new Samplable[Double]{
		def sample(implicit r: Random) = (upper - lower) * r.nextDouble
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
}