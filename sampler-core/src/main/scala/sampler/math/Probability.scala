/*
 * Copyright (c) 2012-2013 Crown Copyright 
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

import scala.language.implicitConversions
import scala.math.Numeric.DoubleIsFractional

/** Represents a single probability
 *  
 *  By definition this object holds a positive double value between 0.0 and 1.0
 *  
 *  @constructor Creates a Probability given a double
 *  @param value The numerical value of the Probability, strictly between 0.0 and 1.0
 */

case class Probability(val value: Double){
	assert(value <= 1 && value >= 0, value + "is not a valid probability")
	
	/** Returns the sum of this value and the value of x */
	def + (x: Probability) = Probability(value + x.value)
	
	/** Returns the difference between this value and the value of x */
	def - (x: Probability) = Probability(value - x.value)
	
	/** Returns the product of this value and the value of x */
	def * (x: Probability) = Probability(value * x.value)

	/** Returns the quotient of this value and the value of x */
	def / (x: Probability) = Probability(value / x.value)
}

/** Factory for creating a probability of zero */
object Probability extends ProbabilityImplicits{
    
    /** Instance with a value of 0.0 */
	val zero = Probability(0)
}

trait ProbabilityImplicits{
	/** Implicit conversion of value to a Double */
	implicit def toDouble(p: Probability): Double = p.value
	
	/** Implicit conversions for [[sampler.math.Probability]] instances
	 *  {{{
	 *  import sampler.math.Probability.ProbabilityIsFractional
	 *  
	 *  val p1 = Probability(0.1)
	 *  val p2 = Probability(0.2)
	 *  
	 *  ProbabilityIsFractional.compare(p1, p2)
	 *  ProbabilityIsFractional.plus(p1, p2)
	 *  }}}
	 *  
	 *  Other text
	 */
	implicit object ProbabilityIsFractional extends Fractional[Probability]{
		def compare(x: Probability, y: Probability): Int = DoubleIsFractional.compare(x.value, y.value)
		def plus(x: Probability, y: Probability): Probability = x + y
		def minus(x: Probability, y: Probability): Probability = x - y
		def times(x: Probability, y: Probability): Probability = x * y
		def div(x: Probability, y: Probability): Probability = x / y
		def negate(x: Probability): Probability = Probability(-x.value)
		def fromInt(x: Int): Probability = Probability(x)
		def toInt(x: Probability): Int = x.value.toInt
		def toLong(x: Probability): Long = x.value.toLong
		def toFloat(x: Probability): Float = x.value.toFloat
		def toDouble(x: Probability): Double = x.value
	}
}
