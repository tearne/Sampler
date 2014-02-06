/*
 * Copyright (c) 2012-13 Crown Copyright 
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

import scala.collection.GenSeq
import scala.collection.parallel.ParSeq
import sampler.math.StatisticsComponentImpl
import sampler.math.Statistics
import sampler.Implicits._ 

/** For producing samples from a distribution until a condition is met */

trait Sampler{
//  def kickstart[T](distribution: Distribution[T]): GenSeq[T]
  
  /** Collects samples from distribution until condition returns true
   *  
   *  @param distribution Distribution object from which samples can be drawn
   *  @param condition Function to decide when to stop sampling
   *  @return Collection of sampled elements
   */
	def apply[T](distribution: Distribution[T])(protocol: ConvergenceProtocol[T]): GenSeq[T]
}

trait EmpiricalMetric {
  def distance[T](e1: Empirical[T], e2: Empirical[T]): Double
}

trait MaxMetric extends EmpiricalMetric with StatisticsComponentImpl {
  def distance[T](e1: Empirical[T], e2: Empirical[T]): Double = statistics.maxDistance(e1, e2)
}

//trait MeanMetric extends EmpiricalMetric with StatisticsComponentImpl {
//	def distance[T](e1: Empirical[T], e2: Empirical[T]): Double = statistics.meanDistance(e1, e2)
//}

abstract class ConvergenceProtocol[T](val chunkSize: Int, tolerance: Double){
  this: EmpiricalMetric =>
    
  def converged(seq: GenSeq[T]): Boolean = {
    val e1 = seq.take(seq.size - chunkSize).toEmpiricalSeq
    val e2 = seq.toEmpiricalSeq
    distance(e1, e2) < tolerance
  }
}

/** Serializable implementation of [[sampler.data.Sampler]], uses until method of [[sampler.data.Distribution]] 
 *  to sample from the distribution
 */
class SerialSampler extends Sampler with Serializable{
	def apply[T](distribution: Distribution[T])(protocol: ConvergenceProtocol[T]) = {
	  val chunkSize = protocol.chunkSize
	  
	  def takeMore(previous: Seq[T]): Seq[T] = {
		  if(protocol.converged(previous)) previous
		  else takeMore (
		      previous ++ (distribution.until(_.length > chunkSize).sample())
		  )
	  }
	  
	  takeMore(distribution.until(_.length > chunkSize).sample())
	}
}

/** Parallelised implementation of [[sampler.data.Sampler]] allowing sampling of a distribution across
 *  multiple threads, which continuously takes batches of samples from the supplied distribution until
 *  a condition is met
 *  
 *  Warning: The supplied distribution must be thread safe to avoid propagation of error resulting from the 
 *  concurrent calling of the sample method. E.g. In the Commons math normal distribution parallel sampling 
 *  can sometimes give NaN due to concurrent access to a random number generator
 *  {{{
*  val normal = new NormalDistribution(0,0.1)
*  (1 to 1000000000).par.foreach{i =>
*      val r = normal.sample
*      if(r.isNaN()) throw new Exception("r = "+r)
*  }
*  }}}
*  Example taken from http://stackoverflow.com/questions/20969292/thread-safety-warnings
 *  
 *  @constructor Create a new ParrallerSampler 
 *  @param chunkSize Number of samples taken during each iteration
 */
class ParallelSampler extends Sampler{
	def apply[T](distribution: Distribution[T])(protocol: ConvergenceProtocol[T]) = {
	  val chunkSize = protocol.chunkSize
	  
	  def takeMore(previous: ParSeq[T]): ParSeq[T] = {
			if(protocol.converged(previous)) previous
			else takeMore(
					previous ++ (1 to chunkSize).par.map(i => distribution.sample)
			)
		}
		val kickstart = (1 to chunkSize).par.map(i => distribution.sample)
		takeMore(kickstart)
	}
}
