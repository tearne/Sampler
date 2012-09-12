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

import sampler.math.Random
import scala.annotation.tailrec
import sampler.fit.ABCModel
import sampler.data.Distribution
import sampler.fit.Prior
import sampler.fit.ABC
import sampler.io.CSVTableWriter
import java.nio.file.Paths
import sampler.data.Types.Column
import sampler.math.Probability
import sampler.r.ScriptRunner
import sampler.data.WeightsTable
import sampler.run.agent.LocalActorRunner
import sampler.data.Types._
import scala.annotation.tailrec
import sampler.data.FrequencyTableBuilder
import sampler.data.Distance
import sampler.data.FrequencyTable

trait WithoutReplacementABCModel{
	/*
	 * On first sample of an infected population work out the uncertainty around
	 * the sample prevalence.  Then, supposing the prevalence subsequently increases
	 * by alpha (absolute number of infected animals), work out the minimum number
	 * of samples to have 95% confidence of detecting an increase, given all the
	 * uncertainty thus far.
	 */
	
	//TODO: Introduce imperfect tests
	//TODO: Mixture model for prevalence, to allow for zero?
	
	/*
	 * 
	 * 	Setup
	 *  - Create model sampling distribution
	 *  - Build ABC harness
	 * 
	 */
	
	val populationSize: Int
	val sampleSize: Int
	implicit val r: Random
	
	def modelDistribution(numInfected: Int, sampleSize: Int, populationSize: Int) = {
		val population = (1 to populationSize).map(_ <= numInfected)
		Distribution.withoutReplacement(population, sampleSize).map(sample =>
			sample.count(identity)
		)
	}
	
	class Model(populationSize: Int, sampleSize: Int)  extends ABCModel{
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
	}
	
	val model = new Model(populationSize, sampleSize)
	import model._
	
	// Uniform prior for true number infected [0,populationSize]
	val uniformPrior = new Prior[Parameters]{
		def density(p: Parameters) = {
			if(p.numInfected > populationSize || p.numInfected < 0) 0
			else 1.0 / populationSize
		}
		def sample(implicit r: Random) = 
			Parameters(r.nextInt(1 + populationSize))
	}
	
	def getPosterior(numPosObserved: Int) = {
		val runner = new LocalActorRunner()
		
		val posterior = ABC.apply(model, r)(
				uniformPrior,
				Observations(numPosObserved),
				reps = 10,
				particles = 10000,
				startTolerance = 1,
				refinementAttempts = 4,
				runner,
				None
		)
		
		runner.shutdown
		posterior
	}
}

trait Environment{
	val workingDir = Paths.get("examples").resolve("PrevalenceChange")
	implicit val r = new Random()
}
	
	
	
object PrevChangeApp extends App with WithoutReplacementABCModel with Environment{
	
	val populationSize = 60
	val sampleSize = 30
	val numPositiveObservations = 10
	import model._

	//Get a posterior for the true number of infected given sample so far
	val posterior = getPosterior(numPositiveObservations)
	
	//Work out what power a new sample would have to identify a 
	//prevalence change with the desired  confidence
	val requiredConfidence = Probability(0.80)
	val mcChunkSize = 100
	val mcConvergence = 0.001
	
	def smallestDetectablePrevIncrease(newSampleSize: Int): Option[Double] = {
		def confidenceInDetection(extraNumInf: Int): Probability = {
			val restrictedPosterior = posterior
					.discardWeights
					.filter(_.numInfected < populationSize - extraNumInf)
					.map(_.numInfected)
					
			val differenceDist = new Distribution[Int]{
				def sample(implicit r: Random) = {
					val numInfectedPast = restrictedPosterior.sample
					val numInfectedPresent = numInfectedPast + extraNumInf
					modelDistribution(numInfectedPresent, newSampleSize, populationSize).sample - numInfectedPast
				}
			} 
					
			val result = FrequencyTableBuilder.parallel(differenceDist, mcChunkSize){samples =>
				val distance = Distance.max(FrequencyTable(samples.seq.take(samples.size - mcChunkSize)), FrequencyTable(samples.seq))
				(distance < mcConvergence) || (samples.size > 1e8)
			}.rightTail(0)
			println("Conf for increase of "+extraNumInf+" is "+result)
			result
		}
		
		val prevIncreaseSeq = (1 to populationSize/2)
		
		prevIncreaseSeq.map(i => (i,i))
			.view
			.map{case (inc, jnc) => (confidenceInDetection(inc), jnc)}
			.find(_._1 > requiredConfidence)
			.map(_._2)
	}
	
	// Choose a range of follow-up sample sizes
	val secondSampleSize = (50 to 60) by 2
	val minDetectableOption = secondSampleSize.map(smallestDetectablePrevIncrease)
	
	val power = (secondSampleSize zip minDetectableOption)
			.filter{case (ss, md) => md.isDefined}.map{case (ss, md) => (ss,md.get)}
	
	import sampler.data.Types._
	new CSVTableWriter(workingDir.resolve("SamplePower.csv"), true)(
		Column(power.map(_._1), "SampleSize"),
		Column(power.map(_._2), "minDetectable")
	)
	
	// Plot a graph in R
	val script = """
require(ggplot2)
require(reshape)
		
values = read.csv("SamplePower.csv")
pdf("SamplePower.pdf", width=8.27, height=5.83)
ggplot(values, aes(x=SampleSize, y=minDetectable)) + geom_line()
dev.off()
"""
	ScriptRunner(script, workingDir.resolve("script.r"))
}