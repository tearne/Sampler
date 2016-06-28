///*
// * Copyright (c) 2012-13 Crown Copyright 
// *                    Animal Health and Veterinary Laboratories Agency
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package sampler.olddata
//
//import scala.collection.GenSeq
//import scala.collection.parallel.ParSeq
//import sampler.math.Statistics
//import sampler.empirical.Empirical
//
///** For producing samples from a distribution until a condition is met */
//
//trait Sampler{
//  /** Collects samples from distribution until condition returns true
//   *  
//   *  @param distribution Distribution object from which samples can be drawn
//   *  @param condition Function to decide when to stop sampling
//   *  @return Collection of sampled elements
//   */
//	def apply[T](distribution: Distribution[T])(protocol: ConvergenceProtocol[T]): GenSeq[T] 
//}
//
///** Metric defining how the distance between two Empirical objects should be measures */
//trait EmpiricalMetric {
//  def distance[T](e1: Empirical[T], e2: Empirical[T]): Double
//}
//
///** Implementation of [[sampler.data.EmpiricalMetric]] using the maximum distance statistical method */ 
//trait MaxMetric extends EmpiricalMetric with Statistics{
//  def distance[T](e1: Empirical[T], e2: Empirical[T]): Double 
//  	= maxDistance(e1, e2)
//}
//
//trait MeanMetric extends EmpiricalMetric with Statistics{
//	def distance[T: Fractional](e1: Empirical[T], e2: Empirical[T]): Double 
//		= meanDistance(e1, e2)
//}
//
///** Determines whether a sequence of samples has converged. Requires mixin of a [[sampler.data.EmpiricalMetric]] 
// *  implementation
// *  
// *  @constructor Create a new convergence protocol given a chunk size and tolerance
// *  @param chunkSize the size of sampled chunks that have been used to build the sequence of samples
// *  @param tolerance the degree to which the samples can differ whilst being classed as converged
// *  @param maxRetries the number of samples to try be exiting without meeting convergence criteria
// */
//abstract class ConvergenceProtocol[T](val chunkSize: Int, tolerance: Double, maxRetries: Int){
//  this: EmpiricalMetric =>
//    
//  /** Tests a sequence of samples for convergence
//   * 
//   *  @param seq Samples to be tested for convergence
//   *  @return true/false according to if the samples had converged 
//   */
//  def converged(seq: GenSeq[T]): Boolean = {
//    val e1 = seq.take(seq.size - chunkSize).toEmpiricalSeq
//    val e2 = seq.toEmpiricalSeq
//    distance(e1, e2) < tolerance || seq.size > maxRetries
//  }
//}
//
///** Serializable implementation of [[sampler.data.Sampler]], uses until method of [[sampler.data.Distribution]] 
// *  to sample from the distribution
// *  
// *  {{{
// *  scala> import sampler.math.Random
// *  scala> import sampler.data._
// *  
// *  scala> implicit val r: Random = Random
// *  scala> val dist = Distribution.uniform(0, 10)
// *  dist: sampler.data.Distribution[Int] = sampler.data.Distribution$$anon9@f4734e
// *  
// *  scala> SerialSampler.apply(dist)(new ConvergenceProtocol[Int](10, 0.5, 100000) with MaxMetric)
// *  res0: Seq[Int] = Vector(7, 1, 9, 9, 9, 6, 0, 8, 6, 7)
// *  }}}
// */
//object SerialSampler extends Sampler with Serializable{
//	def apply[T](distribution: Distribution[T])(protocol: ConvergenceProtocol[T]) = {
//	  val chunkSize = protocol.chunkSize
//	  
//	  def takeMore(previous: Seq[T]): Seq[T] = {
//		  if(protocol.converged(previous)) previous
//		  else takeMore (
//		      previous ++ (distribution.until(_.length == chunkSize).sample())
//		  )
//	  }
//	  
//	  takeMore(distribution.until(_.length == chunkSize).sample())
//	}
//}
//
///** Parallelised implementation of [[sampler.data.Sampler]] allowing sampling of a distribution across
// *  multiple threads, which continuously takes batches of samples from the supplied distribution until
// *  a condition is met
// *  
// *  {{{
// *  scala> import sampler.math.Random
// *  scala> import sampler.data._
// *  
// *  scala> implicit val r: Random = Random
// *  scala> val dist = Distribution.uniform(0, 10)
// *  dist: sampler.data.Distribution[Int] = sampler.data.Distribution$$anon9@f4734e
// *  
// *  scala> ParallelSampler.apply(dist)(new ConvergenceProtocol[Int](10, 0.5, 100000) with MaxMetric)
// *  res0: scala.collection.parallel.ParSeq[Int] = ParVector(3, 2, 7, 9, 1, 3, 7, 6, 7, 4)
// *  }}}
// *  
// *  Warning: The supplied distribution must be thread safe to avoid propagation of error resulting from the 
// *  concurrent calling of the sample method. E.g. In the Commons math normal distribution parallel sampling 
// *  can sometimes give NaN due to concurrent access to a random number generator
// * {{{
//*  val normal = new NormalDistribution(0,0.1)
//*  (1 to 1000000000).par.foreach{i =>
//*      val r = normal.sample
//*      if(r.isNaN()) throw new Exception("r = "+r)
//*  }
//*  }}}
//*  Example taken from http://stackoverflow.com/questions/20969292/thread-safety-warnings
// *  
// *  @constructor Create a new ParrallerSampler 
// *  @param chunkSize Number of samples taken during each iteration
// */
//object ParallelSampler extends Sampler{
//	def apply[T](distribution: Distribution[T])(protocol: ConvergenceProtocol[T]) = {
//	  val chunkSize = protocol.chunkSize
//	  
//	  def takeMore(previous: ParSeq[T]): ParSeq[T] = {
//			if(protocol.converged(previous)) previous
//			else takeMore(
//					previous ++ (1 to chunkSize).par.map(i => distribution.sample)
//			)
//		}
//		takeMore((1 to chunkSize).par.map(i => distribution.sample))
//	}
//}
