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

package sampler.util

import sampler.data.Empirical
import sampler.data.Samplable
import scala.annotation.tailrec
import scala.collection.GenSeq
import sampler.math.Random

trait Implicits {
	implicit def seqToRich[T](seq: Seq[T]) = new RichSeq(seq)
	class RichSeq[T](seq: Seq[T]){
		def toEmpirical() = new Empirical(seq.toIndexedSeq)
	}
}