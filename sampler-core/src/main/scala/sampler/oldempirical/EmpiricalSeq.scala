package sampler.oldempirical;
///*
// * Copyright (c) 2012 Crown Copyright 
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
//package sampler.empirical
//
//import sampler.math.Random
//import scala.collection.GenTraversableOnce
//
///** Empirical implementation which is backed by an IndexedSeq.
// * 
// * Ideal for collecting observations from continuous distributions or distributions 
// * with few repeated values.
// */
//class EmpiricalSeq[A](val values: IndexedSeq[A]) extends Empirical[A]{ self =>
//  
//  lazy val size = values.size
//  
//  /** A map from each observation to the probability of seeing that value */
//  lazy val probabilityTable = {
//		val sizeAsDouble = values.size.asInstanceOf[Double]
//		values.groupBy(identity).map{case (k,v) => (k, v.size / sizeAsDouble)}
//	}
//	
//  /** Returns a new Empirical containing all the observations in this instance plus those in the
//   *  more instance
//   *  
//   *  {{{
//   *  val empSeq = new EmpiricalSeq(IndexedSeq(1,2,3,4))
//   *  val more = IndexedSeq(5,6,7,8)
//   *  
//   *  empSeq ++ more
//   *  }}}
//   *  
//   *  @param more the observations to append
//   *  @return a new empirical containing all observations of this Empirical plus the observations in more
//   */
//  def ++(more: GenTraversableOnce[A]) = new EmpiricalSeq(values ++ more)
//	
//  /** Creates a new [[sampler.data.Samplable]] from the distribution
//   *  
//   *  @return [[sampler.data.Samplable]] object */
////	def toDistribution(implicit r: Random): Distribution[A] = DistributionBuilder.uniform(values)
//
//	override def canEqual(other: Any): Boolean = other.isInstanceOf[EmpiricalSeq[_]]
//}
