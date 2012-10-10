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

package sampler.examples.prevchange

import sampler.data._
import sampler.math.Random
import sampler.fit.ABCModel
import scala.annotation.tailrec
import sampler.fit.Prior
import sampler.fit.ABCComponent

/*
 * This ABC model will fit the underlying number of infected units in a population
 * of fixed size, when a single sample has been taken
 */
trait WithoutReplacementABC {
	
	//TODO: Introduce imperfect tests
	//TODO: Adapt for a series of observations with different sample sizes
	
	implicit val r: Random
	
	def modelDistribution(numInfected: Int, sampleSize: Int, populationSize: Int) = {
		val population = (1 to populationSize).map(_ <= numInfected)
		Samplable.withoutReplacement(population, sampleSize).map(sample =>
			sample.count(identity)
		)
	}
	
	class Model(val populationSize: Int, val sampleSize: Int)  extends ABCModel{
		case class Parameters(numInfected: Int) extends ParametersBase {
			def perturb() = {
				@tailrec
				def pertInRange(n: Int): Int = {
					val m = n + List(-2,-1,0,1,2)(r.nextInt(5))
					if(m <= populationSize || m >= 0) m
					else pertInRange(n)
				}
				Parameters(pertInRange(numInfected))
			}
			
			def perturbDensity(that: Parameters)={
				val diff = that.numInfected - numInfected
				if(diff > 1 || diff < 0) 0.0
				else 1.0 /3
			}
		}
		
		case class Observations(numPositive: Int) extends ObservationsBase
		
		//TODO smarter way to do this!
		implicit object ParametersAreFractional extends Fractional[Parameters]{
			implicit def intToParams(i: Int) = Parameters(i)
			def div(x: Parameters, y: Parameters): Parameters = x.numInfected / y.numInfected
			def plus(x: Parameters, y: Parameters): Parameters = x.numInfected + y.numInfected
			def minus(x: Parameters, y: Parameters): Parameters = x.numInfected - y.numInfected
			def times(x: Parameters, y: Parameters): Parameters = x.numInfected * y.numInfected
			def negate(x: Parameters): Parameters = -x.numInfected
			def fromInt(x: Int): Parameters = x.numInfected
			def toInt(x: Parameters): Int = x.numInfected.toInt
			def toLong(x: Parameters): Long = x.numInfected.toLong
			def toFloat(x: Parameters): Float = x.numInfected.toFloat
			def toDouble(x: Parameters): Double = x.numInfected.toDouble
			def compare(x: Parameters, y: Parameters): Int = x.numInfected - y.numInfected
		}
		
		case class Output(numPositive: Int) extends OutputBase{
			def closeToObserved(obs: Observations, tolerance: Double) = {
				math.abs(numPositive - obs.numPositive) < tolerance
			}
		}
		
		def init(p: Parameters, obs: Observations) = 
			modelDistribution(p.numInfected, sampleSize, populationSize).map(pos => Output(pos))
			
		// Uniform prior for true number infected [0,populationSize]
		val uniformPrior = new Prior[Parameters]{
			def density(p: Parameters) = {
				if(p.numInfected > populationSize || p.numInfected < 0) 0
				else 1.0 / populationSize
			}
			def sample(implicit r: Random) = 
				Parameters(r.nextInt(1 + populationSize))
		}
	}
}