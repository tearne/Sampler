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
import scala.collection.GenSeq
import scala.collection.parallel.ParSeq
import scala.math.Numeric.DoubleIsFractional
import sampler.math.Probability

/*
 * Anything from which we can draw samples.  E.g. an analytical distribution,
 * or bootstrapping from a data set of observations
 */
trait Samplable[+A] extends Serializable{ 
	self =>
		
	def sample(): A
	
	/*
	 * Note:
	 * 
	 * Calling the methods below will always return a Samplable, not necessarily 
	 * the same type as the original implementation.  This is because it's 
	 * unclear how, for example, how a backing collection within an Emprical 
	 * should be transformed after an 'until' operation which requires a sequence 
	 * of specific values in a row; every possible observation would form too
	 * large a set.  
	 * 
	 * Furthermore, it's difficult to see how to set up a uniform builder 
	 * signature for all implementation classes.  E.g. some implementors may use
	 * tables of counts, weights, or just seq of values whic would need 
	 * transforming.
	 */
	
	def until(condition: IndexedSeq[A] => Boolean) = new Samplable[IndexedSeq[A]]{
		def sample() = {
			@tailrec
			def append(previous: IndexedSeq[A]): IndexedSeq[A] = {
				if(condition(previous)) previous
				else append(previous.:+(self.sample()))
			}
			append(IndexedSeq[A](self.sample()))
		}
	}
	
	def filter(predicate: A => Boolean) = new Samplable[A]{
		def sample() = {
			@tailrec
			def tryAgain(): A = {
				val s = self.sample()
				if(predicate(s)) s
				else tryAgain()
			}
			
			tryAgain()
		}
	}
	
	def map[B](f: A => B) = new Samplable[B]{
		def sample() = f(self.sample())
	}
	
	def flatMap[B](f: A => Samplable[B]) = new Samplable[B]{
		def sample() = f(self.sample()).sample()
	}
	
  def combine[B, C](that: Samplable[B], op: (A, B) => C) = new Samplable[C]{
		def sample() = op(self.sample(), that.sample())
	}
	
  def convolve[B >: A](that: Samplable[B])(implicit n: Numeric[B]) = combine(that, n.plus _)
    def crossCorrelate[B >: A](that: Samplable[B])(implicit n: Numeric[B]) = combine(that, n.minus _)
}

object Samplable{
	def diracDelta[T](value: T) = new Samplable[T]{
		def sample() = value
	}
	
	def uniform(lower: Double, upper: Double) = new Samplable[Double]{
		val r = new Random()
		def sample() = (upper - lower) * r.nextDouble()
	}
	
	def uniform[T](items: IndexedSeq[T]) = new Samplable[T]{
		val r = new Random()
		val size = items.size
		def sample() = items(r.nextInt(size))
	}
	
	def withoutReplacement[T](items: IndexedSeq[T], sampleSize: Int) = new Samplable[List[T]]{
		val r = new Random()
		def sample() = {
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
	
	def binaryPopulation(numInfected: Int, size: Int) = new Samplable[Boolean]{
		val r = new Random()
		def sample() = r.nextInt(size) < numInfected
	}
	
	def normal(mean:Double, variance: Double) = new Samplable[Double]{
		val r = new Random()
		val d = new NormalDistribution(0,variance)
		def sample() = d.sample
		def density(value: Double) = d.density(value)
	}
	
	def bernouliTrial(probSuccess: Samplable[Probability]) = new Samplable[Boolean]{
    val r = new Random()
	  def sample() = r.nextBoolean(probSuccess.sample)
	}
	
	def coinToss() = new Samplable[Boolean] {
    val r = new Random()
	  def sample() = r.nextBoolean(Probability(0.5))
	}
}

trait SampleBuilder{
	def apply[T](samplable: Samplable[T])(condition: GenSeq[T] => Boolean): GenSeq[T]
}

object SerialSampleBuilder extends SampleBuilder with Serializable{
	def apply[T](samplable: Samplable[T])(condition: GenSeq[T] => Boolean) = 
		samplable.until(condition).sample()
}

class ParallelSampleBuilder(chunkSize: Int) extends SampleBuilder{
	def apply[T](samplable: Samplable[T])(condition: GenSeq[T] => Boolean) = {
		def takeMore(previous: ParSeq[T]): ParSeq[T] = {
			if(condition(previous)) previous
			else takeMore(
					previous ++ (1 to chunkSize).par.map(i => samplable.sample)
			)
		}
		val kickstart = (1 to chunkSize).par.map(i => samplable.sample)
		takeMore(kickstart)
	}
}

trait SampleBuilderComponent{
	val builder: SampleBuilder
}
