/*
 * Copyright (c) 2012-15 Crown Copyright 
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

import scala.IndexedSeq
import scala.annotation.tailrec

import sampler.math.AliasTable
import sampler.math.Partition
import sampler.math.Random
import spire.math.Fractional

/** Trait for objects from which we can draw samples
 * 
 *  Can be used for analytical distributions or bootstrapping from a data set of 
 *  observations, among other things.*/
trait Distribution[+A] extends Serializable{ 
	self =>
	
	/** Sample from the underlying collection
	 * 
	 *  @return Sampled value */
	def sample(): A
	
	/** Samples until a condition is met. Return value includes the sample when the 
	 *  condition was met */ 
	def until(predicate: IndexedSeq[A] => Boolean) = new Distribution[IndexedSeq[A]]{
		def sample() = {
			@tailrec
			def append(previous: IndexedSeq[A]): IndexedSeq[A] = {
				if(predicate(previous)) previous
				else append(previous.:+(self.sample()))
			}
			append(IndexedSeq[A](self.sample()))
		}
	}
	
	/** Builds a new [[sampler.data.Distribution]] containing only samples which satisfy 
	 *  the predicate */
	def filter(predicate: A => Boolean) = new Distribution[A]{
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
	
	/** Builds a new [[sampler.data.Distribution]] by mapping sampled values. */
	def map[B](f: A => B) = new Distribution[B]{
		def sample() = f(self.sample())
	}
	
	/** Builds a new [[sampler.data.Distribution]] by first sampling from this
	 *  distribution and then using the value to sample from the parameterised 
	 *  distribution f.
	 *  
	 *  @tparam B the return type of the parameterise distribution f.
	 *  @param f the distribution parameterised by this distibution. */
	def flatMap[B](f: A => Distribution[B]) = new Distribution[B]{
		def sample() = f(self.sample()).sample()
	}
	
	/** Builds a new [[sampler.data.Distribution]] using a binary function to combine this 
	 *  [[sampler.data.Distribution]] with another.
	 *  
	 *  @tparam C the element type of the returned object
	 *  @param op the function to combine the elements of this and that [[sampler.data.Distribution]]
	 *  @param that the other distribution to be combined with
	 *  @return a new [[sampler.data.Distribution]] resulting from applying the binary function to 
	 *  sampled values. */
	def combine[B, C](that: Distribution[B])(op: (A, B) => C) = new Distribution[C]{
		def sample() = op(self.sample(), that.sample())
	}
	
	/** Builds a new [[sampler.data.Distribution]] by adding the samples of one 
	 *  [[sampler.data.Distribution]] to the samples of another. */
	def convolve[B >: A](that: Distribution[B])(implicit n: Numeric[B]) = combine(that)(n.plus _)
    
	/** Builds a new [[sampler.data.Distribution]] by subtracting the samples of another 
	 *  [[sampler.data.Samplable]] from the samples of this. */
	def crossCorrelate[B >: A](that: Distribution[B])(implicit n: Numeric[B]) = combine(that)(n.minus _)
	
	def +[B >: A](that: Distribution[B])(implicit f: Fractional[B]) = combine(that)(f.plus _)
	def -[B >: A](that: Distribution[B])(implicit f: Fractional[B]) = combine(that)(f.minus _)
	def *[B >: A](that: Distribution[B])(implicit f: Fractional[B]) = combine(that)(f.times _)
	def /[B >: A](that: Distribution[B])(implicit f: Fractional[B]) = combine(that)(f.div _)
	
	def +[B >: A](constant: B)(implicit f: Fractional[B]) = map(sampled => f.plus(sampled, constant))
	def -[B >: A](constant: B)(implicit f: Fractional[B]) = map(sampled => f.minus(sampled, constant))
	def *[B >: A](constant: B)(implicit f: Fractional[B]) = map(sampled => f.times(sampled, constant))
	def /[B >: A](constant: B)(implicit f: Fractional[B]) = map(sampled => f.div(sampled, constant))
}

object Distribution{
	/** Builds a [[sampler.data.Distribution]] which delegates sampling to the 
	 *  call-by-name arg.
	 * 
	 * @param function supplying the sampled value */
	def apply[T](f: => T) = new Distribution[T]{
		def sample() = f
	}
	
	/** Builds a [[sampler.data.Distribution]] which always samples the same value.
	 *  
	 *  @param value the value to be returned when sampled */
	def continually[T](value: T) = new Distribution[T]{
		def sample() = value
	}
	
	/** Builds a [[sampler.data.Distribution]] which represents a Uniform distribution, 
	 *  of doubles
	 *  
	 *  @param lower the lower bound of the distribution (inclusive)
	 *  @param upper the upper bound of the distribution (exclusive) */
	def uniform(lower: Double, upper: Double)(implicit r: Random) = new Distribution[Double]{
		def sample() = (upper - lower) * r.nextDouble() + lower
	}
	
	/** Builds a [[sampler.data.Distribution]] which represents a Uniform distribution 
	 *  of integers between the lower (inclusive) and upper (exclusive) values
	 *  
	 *  @param lower the lower bound of the distribution (inclusive)
	 *  @param upper the upper bound of the distribution (exclusive) */
	def uniform(lower: Int, upper: Int)(implicit r: Random) = new Distribution[Int]{
		def sample() = r.nextInt(upper - lower) + lower
	}
	
	/** Builds a [[sampler.data.Distribution]] which allows sampling from a an indexed
	 *  sequence of values with uniform weighting.
	 *  
	 *  @param items the sequence of values to be sampled from */
	def uniform[T](items: IndexedSeq[T])(implicit r: Random) = new Distribution[T]{
		val size = items.size
		def sample() = items(r.nextInt(size))
	}
	
	/** Builds a [[sampler.data.Distribution]] which samples multiple 
	 *  values without replacement with uniform weighting.
	 *  
	 *  @param items the sequence of values to be sampled from 
	 *  @param sampleSize the number of items to be selected from the set
	 *  
	 *  @example
	 *  {{{
	 *  implicit val r = Random
	 *  val model = Distribution.withoutReplacement(IndexedSeq("red", "blue", "green", "yellow"), 2)
	 *  
	 *  model.sample
	 *  res1: List[String] = List(blue, green)
	 *  }}}
	 *  */
	//TODO reverse list?
	def withoutReplacement[T](items: IndexedSeq[T], sampleSize: Int)(implicit r: Random) = new Distribution[List[T]]{
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
	
	//TODO test, urgently!
	//TODO reverse list?
	def withoutReplacement(
			populationSize: Int, 
			populationTrue: Int, 
			stopWhen: IndexedSeq[Boolean] => Boolean = _ => false
	)(implicit r: Random) = new Distribution[IndexedSeq[Boolean]]{
		assert(populationSize >= populationTrue)
		def sample() = {
			@tailrec def take(acc: IndexedSeq[Boolean], nTrue: Int, size: Int): IndexedSeq[Boolean] = {
				if(size ==0 || stopWhen(acc)) acc
				else {
					val item = r.nextInt(size) <= nTrue
					take(acc :+ item, if(item) nTrue - 1 else nTrue, size - 1)
				}
			}
			take(IndexedSeq.empty[Boolean], populationSize, populationTrue)
		}
	}

	/** Builds a [[sampler.data.Distribution]] to flip coins.
	 *  
	 *  @param probSuccess the probability of success when sampling from this object */
	def bernoulli(probSuccess: Double)(implicit r: Random) = new Distribution[Boolean]{
	  def sample() = r.nextBoolean(probSuccess)
	}
	
	//TODO test
	def binomial(probSuccess: Double, trials: Double)(implicit r: Random) = 
		bernoulli(probSuccess)(r)
			.until(_.size == trials)
			.map(_.count(identity))
			
	//TODO test
	def negativeBinomial(numFailures: Int, probSuccess: Double)(implicit r: Random) = {
		bernoulli(probSuccess)(r)
			.until(_.count(!_) == numFailures)
			.map(_.size)
	}
	
	//TODO test
	def geometric(probSuccess: Double)(implicit r: Random) = 
		bernoulli(probSuccess)(r)
			.until(_.last)
			.map(_.size)
	
	//TODO test
	def hypergeometric(trials: Int, populationSize: Int, populationSuccesses: Int)(implicit r: Random) =
		withoutReplacement(populationSize, populationSuccesses,_.size == trials)
			.map(_.count(identity))
		
	//TODO test
	def exponential(rate: Double)(implicit r: Random) = 
		uniform(0,1).map(x => - math.log(x) / rate)
			
	//TODO test
	def poisson(rate: Double)(implicit r: Random) = 
		exponential(rate).until(_.sum >= 1).map(_.size - 1)
	
	
	/** Builds a [[sampler.data.Distribution]] to sample from an indexed sequence of values
	 *  according to the probabilities in a [[sampler.math.Partition]].  Uses the alias method: 
	 *  [[sampler.math.AliasTable]]
	 *  
	 *  @param items the items to be sampled from
	 *  @param p [[sampler.math.Partition]] containing the probabilities of sampling each object
	 */ 
	def fromPartition[T](items: IndexedSeq[T], p: Partition)(implicit r: Random) = new Distribution[T]{
	  private def theSame(a: IndexedSeq[T], b: Partition) = items.size == p.size
	  
	  assert(theSame(items, p), s"Expected both objects to have the same length")
	  
	  val aliasTable = new AliasTable(p)
	  def sample() = items(aliasTable.next(r))
	}
	
	/** Builds a new [[sampler.data.Distribution]] which allows a set of items to be sampled according 
	 *  to a normalised weighting.
	 *  
	 *  @param items the items which are to be sampled
	 *  @param p [[sampler.math.Partition]] containing the probabilities of sampling each object
	 */ 
	def fromWeightsTable[T](wTable: Map[T, Double])(implicit r: Random): Distribution[T] = {
		val (parameterSets, weights) = wTable.toIndexedSeq.unzip
		fromPartition(parameterSets, Partition.fromWeights(weights))
	}
}