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
import sampler.math.Partition
import sampler.math.AliasTable

//TODO flatten on samplable

/** Trait for objects from which we can draw samples
 * 
 *  Can be used for analytical distributions or bootstrapping from a data set of observations, 
 *  among other things
 */
trait Samplable[+A] extends Serializable{ 
	self =>
	
	/** Returns a single element of the underlying collection
	 * 
	 *  @return Sampled value */
	def sample(): A
	
	/** Samples from the underlying collection until a condition is met, returns an indexed 
	 *  sequence containing all sampled values inclusive of the sample when the condition was met
	 *  
	 *  @return Sequence of sampled values */ 
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
	
	/** Builds a new [[sampler.data.Samplable]] object containing only values which match 
	 *  the predicate given in the method argument
	 *  
	 *  @param predicate The condition to apply to each element
	 *  @return A new [[sampler.data.Samplable]] resulting from applying the predicate function 
	 *  to select specific elements of this object and collecting the results
	 */
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
	
	/** Builds a new [[sampler.data.Samplable]] by applying a function to all values of the 
	 *  underlying collection
	 *  
	 *  @tparam B the element type of the returned object
	 *  @param f the function to apply to each element
	 *  @return a new [[sampler.data.Samplable]] resulting from applying the given function f 
	 *  to each element of this object and collecting the results. 
	 */
	def map[B](f: A => B) = new Samplable[B]{
		def sample() = f(self.sample())
	}
	
	def flatMap[B](f: A => Samplable[B]) = new Samplable[B]{
		def sample() = f(self.sample()).sample()
	}
	
	/** Builds a new [[sampler.data.Samplable]] by applying a function to combine one 
	 *  [[sampler.data.Samplable]] with another
	 *  
	 *  @tparam C the element type of the returned object
	 *  @param op the function to combine the two elements from the two objects
	 *  @param that the object to be combined with
	 *  @return a new [[sampler.data.Samplable]] resulting from applying the given function to 
	 *  both objects and collecting the results. 
	 */
	def combine[B, C](that: Samplable[B])(op: (A, B) => C) = new Samplable[C]{
		def sample() = op(self.sample(), that.sample())
	}
	
	/** Builds a new [[sampler.data.Samplable]] by adding one [[sampler.data.Samplable]] to another
	 *  
	 *  @param that the object to be added
	 *  @return a new [[sampler.data.Samplable]] resulting from adding two objects together and 
	 *  collecting the results. 
	 */
	def convolve[B >: A](that: Samplable[B])(implicit n: Numeric[B]) = combine(that)(n.plus _)
    
	/** Builds a new [[sampler.data.Samplable]] by subtracting one [[sampler.data.Samplable]] 
	 *  from another
	 *  
	 *  @param that the object to be subtracted
	 *  @return a new [[sampler.data.Samplable]] resulting from subtracting two objects and 
	 *  collecting the results. 
	 */
	def crossCorrelate[B >: A](that: Samplable[B])(implicit n: Numeric[B]) = combine(that)(n.minus _)
}

object Samplable{
	/** Builds a new [[sampler.data.Samplable]] which always returns the same value when sampled
	 *  
	 *  @param value the value to be returned when sampled */
	def continually[T](value: T) = new Samplable[T]{
		def sample() = value
	}
	
	/** Builds a new [[sampler.data.Samplable]] which represents a Uniform distribution, 
	 *  provides samples as double values between the supplied lower inclusive and upper 
	 *  exclusive values
	 *  
	 *  @param lowerInclusive the lower bound of the distribution (inclusive)
	 *  @param upperExclusive the upper bound of the distribution (exclusive) */
	def uniform(lowerInclusive: Double, upperExclusive: Double)(implicit r: Random) = new Samplable[Double]{
		def sample() = (upperExclusive - lowerInclusive) * r.nextDouble() + lowerInclusive
	}
	
	/** Builds a new [[sampler.data.Samplable]] which represents a Uniform distribution, 
	 *  provides samples as integer values between the supplied lower inclusive and upper 
	 *  exclusive values
	 *  
	 *  @param lowerInclusive the lower bound of the distribution (inclusive)
	 *  @param upperExclusive the upper bound of the distribution (exclusive) */
	def uniform(lowerInclusive: Int, upperExclusive: Int)(implicit r: Random) = new Samplable[Int]{
		def sample() = r.nextInt(upperExclusive - lowerInclusive) + lowerInclusive
	}
	
	/** Builds a new [[sampler.data.Samplable]] which allows sampling from a finite sequence 
	 *  of values with equal probability
	 *  
	 *  @param items the sequence of values to be sampled from */
	def uniform[T](items: IndexedSeq[T])(implicit r: Random) = new Samplable[T]{
		val size = items.size
		def sample() = items(r.nextInt(size))
	}
	
	/** Builds a new [[sampler.data.Samplable]] which allows sampling  of multiple 
	 *  values from a finite sequence of values with equal probability
	 *  
	 *  @param items the sequence of values to be sampled from 
	 *  @param sampleSize the number of items to be selected from the set
	 *  
	 *  @example
	 *  {{{
	 *  implicit val r = Random
	 *  val model = Samplable.withoutReplacement(IndexedSeq("red", "blue", "green", "yellow"), 2)
	 *  
	 *  model.sample
	 *  res1: List[String] = List(blue, green)
	 *  }}}
	 *  */
	def withoutReplacement[T](items: IndexedSeq[T], sampleSize: Int)(implicit r: Random) = new Samplable[List[T]]{
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
	
	/** Builds a new [[sampler.data.Samplable]] which represents a finite population with a 
	 *  given number of individuals with a certain characteristic. When sampled a boolean value
	 *  is returned which determines if an member of the population with the characteristic of 
	 *  interest was selected
	 *  
	 *  @param numInfected the number of individuals with the given characteristic (e.g. infected) 
	 *  @param size the total size of the population
	 *  */
	def binaryPopulation(numInfected: Int, size: Int)(implicit r: Random) = new Samplable[Boolean]{
		def sample() = r.nextInt(size) < numInfected
	}
	
	/** Builds a new [[sampler.data.Samplable]] which samples from a Normal distribution
	 *  
	 *  @param mean the mean of the distribution 
	 *  @param sd the Standard Deviation of the distribution
	 *  */
	def normal(mean:Double, sd: Double)(implicit r: Random) = new Samplable[Double]{
		val d = new NormalDistribution(mean,sd)
		def sample() = d.sample
		def density(value: Double) = d.density(value)
	}
	
	/** Builds a new [[sampler.data.Samplable]] which represents a Bernouli distribution.
	 *  
	 *  @param probSuccess the probability of success when sampling from this object */
	def bernouliTrial(probSuccess: Probability)(implicit r: Random) = new Samplable[Boolean]{
	  def sample() = r.nextBoolean(probSuccess)
	}
	
	/** Builds a new [[sampler.data.Samplable]] which represents a fair coin */
	def coinToss()(implicit r: Random) = new Samplable[Boolean] {
	  def sample() = r.nextBoolean(Probability(0.5))
	}
	
	/** Builds a new [[sampler.data.Samplable]] which allows a set of items to be sampled according 
	 *  to a given set of probabilities
	 *  
	 *  @param items the items which are to be sampled
	 *  @param p [[sampler.math.Partition]] containing the probabilities of sampling each object
	 */ 
	def fromPartition[T](items: IndexedSeq[T], p: Partition)(implicit r: Random) = new Samplable[T]{
	  private def theSame(a: IndexedSeq[T], b: Partition) = items.size == p.size
	  
	  assert(theSame(items, p), s"Expected both objects to have the same length")
	  
	  val aliasTable = new AliasTable(p)
	  def sample() = items(aliasTable.next(r))
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
