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

package sampler.r

/** Allows a sequence of continuous data to be named for plotting 
 *  
 *  @param seq Sequence of samples
 *  @param name Name describing the data
 */
case class NamedSeqFractional[T: Fractional](seq: Seq[T], name: String)

/** Allows a sequence of discrete data to be named for plotting
 *  
 *  @param seq Sequence of samples
 *  @param name Name describing the data
 */
case class NamedSeqIntegral[T: Integral](seq: Seq[T], name: String)

/** Implicits for naming sequences of data when passing to 
 *  [[sampler.r.QuickPlot]]
 *  
 *  {{{
 *  > import sampler.Implicits._
 *  > IndexedSeq(1,2,3).discrete("Integers")
 *      res: sampler.r.NamedSeqIntegral[Int] = NamedSeqIntegral(Vector(1, 2, 3),Integers)
 *      
 *  > IndexedSeq(0.1,0.2,0.3).continuous("Doubles")
 *      res4: sampler.r.NamedSeqFractional[Double] = NamedSeqFractional(Vector(0.1, 0.2, 0.3),Doubles)
 *  }}}
 */
trait ToNamedSeq{
	implicit class RichFractionalSeq[T: Fractional](val seq: Seq[T]){
		def continuous(name:String) = NamedSeqFractional(seq, name)
	}

	implicit class RichIntegralSeq[T: Integral](val seq: Seq[T]){
		def discrete(name:String) = NamedSeqIntegral(seq, name)
	}
}