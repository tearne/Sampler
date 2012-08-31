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

package sampler.examples

import scala.annotation.tailrec
import sampler.data.Distribution
import sampler.math.Random
import org.apache.commons.math3.distribution.NormalDistribution
import sampler.fit.Prior
import sampler.fit.ABCModel
import sampler.fit.ABC
import sampler.io.CSVTableWriter
import java.nio.file.Paths
import sampler.data.Types._
import sampler.data.WeightsTable
import sampler.examples.SimpleABC.Model
import sampler.run.ParallelCollectionRunner
import sampler.data.Particle
import sampler.run.SerialRunner

object SimpleABC extends App{
	implicit val random = new Random()
	
	val wd = Paths.get("example").resolve("simpleABC").resolve("data")
	val max = 100.0
	val min = 0.0
	
	class Model extends ABCModel{
		val xObservations = List[Double](2, 37, 98)
		val yObservations = List(60.59811, 1140.96172, 2975.07122)
		
		case class Parameters(m: Double, v: Double) extends ParametersBase{
			val perturbationDist = Distribution.normal(0,1)
			@tailrec
			final def perturb(value: Double): Double = {
				val candidate = value + perturbationDist.sample
				if(candidate <= max && candidate >= min) candidate
				else perturb(value)
			}
			
			def perturb() = Parameters(perturb(m), perturb(v))
			def perturbDensity(that: Parameters) =
				perturbationDist.density(m - that.m) * perturbationDist.density(v - that.v)
		}

		case class Output(outputValues: List[Double]) extends OutputBase{
			def closeToObserved(tolerance: Double): Boolean = {
				val distance = outputValues.zip(yObservations).foldLeft(0.0){case (acc,(s,o)) =>
					acc + math.pow(s - o, 2.0)
				}
				distance < tolerance
			}
		}
		
		def withParameters(p: Parameters) = new Distribution[Output]{
			val modelNoiseDist = Distribution.normal(0,p.v).map(s => s*s)
			override def sample(implicit r: Random) = {
				def f(x: Double) = p.m * x + math.sqrt(x) * modelNoiseDist.sample
				Output(xObservations.map(f))
			}
		}
	}
	
	val model = new Model
	import model._
	
	val prior = new Prior[Parameters]{
		
		def density(p: Parameters) = {
			if(p.m > max || p.m < min || p.v > max || p.v < min) 0.0
			else 1.0 / ((max - min)*(max - min))
		}
		
		def sample(implicit r: Random) = {
			Parameters(
				r.nextDouble(min, max),
				r.nextDouble(min, max)
			)
		}
	}
	
	object Writer extends PopulationWriter{
		def apply(p: WeightsTable[Parameters], tolerance: Double){
			new CSVTableWriter(wd.resolve(tolerance+".csv")).apply(
				Column(p.mapValues(_.m), Some("m")),
				Column(p.mapValues(_.v), Some("v"))
			)
		}
	}
	
	val resultParams = ABC.apply(
			model, random
	)(
			prior, 
			reps = 20, 
			particles = 10000, 
			startTolerance = 1000000,
			refinementAttempts = 20,
			new ParallelCollectionRunner[Particle[Parameters]](),
			Some(Writer)
	)
}

