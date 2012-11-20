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

/*
 * Anything from which we can draw samples.  E.g. an analytical distribution,
 * or bootstrapping from a data set of observations
 */
trait Samplable[+A,-R]{ 
	self =>
		
	def sample(implicit r: R): A
	
	/*
	 * Note:
	 * 
	 * Calling the methods below will always return a Samplable, not necessarily 
	 * the same type as the original implementation.  This is because it's 
	 * unclear how, for example, how a backing collection within an Emprical 
	 * should be transformed after an 'until' operation which requires say two 
	 * specific values in a row; every possible observation would form an
	 * infinite set.  
	 * 
	 * Furthermore, it's difficult to see how to set up a uniform builder 
	 * signature for all implementation classes.  E.g. some implementors may use
	 * tables of counts, weights, or just seq of values whic would need 
	 * transforming.
	 */
	
	def until(condition: IndexedSeq[A] => Boolean) = new Samplable[IndexedSeq[A], R]{
		def sample(implicit r: R) = {
			@tailrec
			def append(previous: IndexedSeq[A]): IndexedSeq[A] = {
				if(condition(previous)) previous
				else append(previous.:+(self.sample(r)))
			}
			append(IndexedSeq[A](self.sample(r)))
		}
	}
	
	def filter(predicate: A => Boolean) = new Samplable[A,R]{
		def sample(implicit r: R) = {
			@tailrec
			def tryAgain(r: R): A = {
				val s = self.sample(r)
				if(predicate(s)) s
				else tryAgain(r)
			}
			
			tryAgain(r)
		}
	}
	
	def map[B](f: A => B) = new Samplable[B,R]{
		def sample(implicit r: R) = f(self.sample(r))
	}
	
	//???? S <: R
	def flatMap[B, S <: R](f: A => Samplable[B,S]) = new Samplable[B,S]{
		def sample(implicit r: S) = f(self.sample(r)).sample(r)
	}
	
	def combine[B, C, S <: R](that: Samplable[B,S], op: Function2[A,B,C]) = new Samplable[C,S]{
		def sample(implicit r: S) = op(self.sample(r), that.sample(r))
	}
	
	def convolve[B >: A, S <: R](that: Samplable[B,S])(implicit n: Numeric[B]) = combine(that, n.plus _)
	def crossCorrelate[B >: A, S <: R](that: Samplable[B,S])(implicit n: Numeric[B]) = combine(that, n.minus _)
}

object Test extends App{
	class Random2 extends Random{
		def nextThingey() = 12
	}
	
	class T
	class S extends T
	val isT: T = new S
	
	val t = new Samplable[T, Random2]{
		def sample(implicit r: Random2) = new T
	}
	val s = new Samplable[S, Random]{
		def sample(implicit r: Random) = new S
	}
	
	val u: Samplable[T,Random] = s
	val w: Samplable[S,Random2] = s
	
	class U
	val p: Samplable[U,Random2] = t.combine(s, (a:T, b:S) => new U)
}

object Samplable{
	import util.{Random => ScalaRandom}
	
	def diracDelta[T](value: T) = new Samplable[T,ScalaRandom]{
		def sample(implicit r: ScalaRandom) = value
	}
	
	def uniform(lower: Double, upper: Double)(implicit r: ScalaRandom) = new Samplable[Double, ScalaRandom]{
		def sample(implicit r: ScalaRandom) = (upper - lower) * r.nextDouble()
	}
	
	def uniform[T](items: IndexedSeq[T])(implicit r: ScalaRandom) = new Samplable[T, ScalaRandom]{
		val size = items.size
		def sample(implicit r: ScalaRandom) = items(r.nextInt(size))
	}
	
	def withoutReplacement[T](items: IndexedSeq[T], sampleSize: Int) = new Samplable[List[T], ScalaRandom]{
		def sample(implicit r: ScalaRandom) = {
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
	
	def binaryPopulation(numInfected: Int, size: Int)(implicit r: ScalaRandom) = new Samplable[Boolean, ScalaRandom]{
		def sample(implicit r: ScalaRandom) = r.nextInt(size) < numInfected
	}
	
	def normal(mean:Double, variance: Double) = new Samplable[Double, ScalaRandom]{
		val d = new NormalDistribution(0,variance)
		def sample(implicit r: ScalaRandom) = d.sample
		def density(value: Double) = d.density(value)
	}
}

trait SampleBuilder{
	def apply[T,Rnd](samplable: Samplable[T,Rnd])(condition: GenSeq[T] => Boolean)(implicit r: Rnd): GenSeq[T]
}

object SerialSampleBuilder extends SampleBuilder{
	def apply[T,Rnd](samplable: Samplable[T, Rnd])(condition: GenSeq[T] => Boolean)(implicit r: Rnd) = {
		samplable.until(condition).sample(r)
	}
}

class ParallelSampleBuilder(chunkSize: Int) extends SampleBuilder{
	def apply[T,Rnd](samplable: Samplable[T, Rnd])(condition: GenSeq[T] => Boolean)(implicit r: Rnd) = {
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