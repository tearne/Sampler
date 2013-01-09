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
import sampler.data._
import sampler.fit._
import sampler.math._
import org.apache.commons.math3.distribution.NormalDistribution
import sampler.fit.Prior
import sampler.io.CSVTableWriter
import java.nio.file.Paths
import sampler.data.Types._
import sampler.examples.SimpleABC.Model
import sampler.run.actor.LocalActorRunner
import sampler.run.SerialRunner

object SimpleABC extends App{
	implicit val random = new Random()
	
	val wd = Paths.get("examples").resolve("simpleABC").resolve("data")
	val max = 100.0
	val min = 0.0
	
	class Model extends ABCModel[Random]{
		case class Parameters(m: Double, v: Double) extends ParametersBase{
			val perturbationDist = Samplable.normal(0,1)
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
		
		case class Observations(x: List[Double], y: List[Double]) extends ObservationsBase
		
		case class Output(outputValues: List[Double]) extends OutputBase{
			def closeToObserved(obs: Observations, tolerance: Double): Boolean = {
				val distance = outputValues.zip(obs.y).foldLeft(0.0){case (acc,(s,o)) =>
					acc + math.pow(s - o, 2.0)
				}
				distance < tolerance
			}
		}
		
		def init(p: Parameters, obs: Observations) = new Samplable[Output,Random]{
			val modelNoiseDist = Samplable.normal(0,p.v).map(s => s*s)
			override def sample(implicit r: Random) = {
				def f(x: Double) = p.m * x + math.sqrt(x) * modelNoiseDist.sample(r)
				Output(obs.x.map(f))
			}
		}
	}
	
	val model = new Model
	import model._
	
	val prior = new Prior[Parameters, Random]{
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
	val obs = Observations(
			List[Double](2, 37, 98), 
			List(60.59811, 1140.96172, 2975.07122)
	)	
	
	object Writer extends PopulationWriter{
		def apply(p: Seq[Parameters], tolerance: Double){
			new CSVTableWriter(wd.resolve(tolerance+".csv"), true).apply(
				Column(p.map(_.m), "m"),
				Column(p.map(_.v), "v")
			)
		}
	}
	
	object ABCRunner extends ABCComponent 
				  with SampleBuilderComponent{
		val builder = SerialSampleBuilder
	}
	
	//val runner = new LocalActorRunner()
	
	val resultParams = ABCRunner(model, random)(
			prior, 
			obs,
			new ABCParameters(20, 1000, 1000000, 20, 500),
			/*runner,*/new SerialRunner,
			Some(Writer)
	)
	
	//runner.shutdown
}

