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

package sampler.math

/** Additional random drawing functionality required within the toolkit */

trait Random {
	def nextDouble(): Double
	def nextInt(n: Int): Int
	
	def nextDouble(min: Double, max: Double): Double = 
    	(max - min) * nextDouble() + min
  
    //TODO test exception
    def nextBoolean(p: Double): Boolean = {
		RangeCheck.probability(p)
		math.random < p
	}
}

/** Instance of Random trait */
object Random extends Random with Serializable{
	def nextDouble = scala.util.Random.nextDouble
	def nextInt(n: Int) = scala.util.Random.nextInt(n)
}

