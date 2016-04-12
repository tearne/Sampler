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