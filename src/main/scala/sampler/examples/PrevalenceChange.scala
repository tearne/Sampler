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

import sampler.math.Random
import scala.annotation.tailrec
import sampler.fit.ABCModel
import sampler.data.Distribution
import sampler.fit.Prior
import sampler.fit.ABC
import sampler.run.ParallelCollectionRunner
import sampler.data.Particle
import sampler.data.FrequencyTableBuilder
import sampler.data.FrequencyTable
import sampler.data.Distance
import sampler.io.CSVTableWriter
import java.nio.file.Paths
import sampler.data.Types.Column
import sampler.run.SerialRunner
import java.io.FileWriter
import java.nio.file.Path
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.io.Source
import sampler.math.Probability
import sampler.run.Runner
import sampler.r.ScriptRunner

object PrevalenceChange extends App {
	/*
	 * On first sample of an infected population work out the uncertainty around
	 * the sample prevalence.  Then, supposing the prevalence subsequently increases
	 * by alpha (absolute number of infected animals), work out the minimum number
	 * of samples to have 95% confidence of detecting an increase, given all the
	 * uncertainty thus far.
	 */
	
	//TODO: Mixture model for prevalence, to allow for zero

	// Provide data on the infection status of sampled animals from a population
	//  - Sampling without replacement
	//  - Unknown prevalance
	//  - Assume test is perfect
	val popSize = 60
	val initialSampleSize = 20
	val posObs = 3
	val reqConfidence = Probability(0.95)
	
	// Init
	val workDir = Paths.get("examples").resolve("PrevalenceChange")
	implicit val r = new Random()
	val mcChunkSize = 100
	val mcConvergence = 0.001
	
	// Build a model of the sampling scenario
	def modelDistribution(numInfected: Int, sampleSize: Int) = {
		val population = (1 to popSize).map(_ < numInfected)
		Distribution.withoutReplacement(population, sampleSize).map(sample =>
			sample.count(identity)
		)
	}
	
	class Model extends ABCModel{
		case class Parameters(numInfected: Int) extends ParametersBase {
			def perturb() = {
				@tailrec
				def pertInRange(n: Int): Int = {
					val m = n + List(-1,0,1)(r.nextInt(3))
					if(m <= popSize || m >= 0) m
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
		
		case class Output(numPositive: Int) extends OutputBase{
			def closeToObserved(tolerance: Double) =
				math.abs(numPositive - posObs) < tolerance
		}
		
		def withParameters(p: Parameters) = 
			modelDistribution(p.numInfected, initialSampleSize).map(pos => Output(pos))
	}
	val model = new Model
	import model._
	
	// Use non-informative prior
	val prior = new Prior[Parameters]{
		def density(p: Parameters) = {
			if(p.numInfected > popSize || p.numInfected < 0) 0
			else 1.0 / popSize
		}
		def sample(implicit r: Random) = 
			Parameters(r.nextInt(1 + popSize))
	}
	
	// Approximate the prevalence posterior	
	val posterior = ABC.apply(model, r)(
			prior,
			reps = 20,
			particles = 10000,
			startTolerance = 10000,
			refinementAttempts = 15,
			Runner.parallelCollection[Particle[Parameters]](),
			None
	)
	
	//Work out what power a subsequent sample has to identify a prevalence change
	def smallestDetectablePrevIncrease(newSampleSize: Int): Option[Double] = {
		def confidenceInDetection(extraNumInf: Int): Probability = {
			val restrictedPosterior = posterior
					.discardWeights
					.filter(_.numInfected < popSize - extraNumInf)
					.map(_.numInfected)
					
			val differenceDist = new Distribution[Int]{
				def sample(implicit r: Random) = {
					val numInfectedPast = restrictedPosterior.sample
					val numInfectedPresent = numInfectedPast + extraNumInf
					modelDistribution(numInfectedPresent, newSampleSize).sample - numInfectedPast
				}
			} 
					
			val result = FrequencyTableBuilder.parallel(differenceDist, mcChunkSize){samples =>
				val distance = Distance.max(FrequencyTable(samples.seq.take(samples.size - mcChunkSize)), FrequencyTable(samples.seq))
				(distance < mcConvergence) || (samples.size > 1e8)
			}.rightTail(0)
			println("Conf for increase of "+extraNumInf+" is "+result)
			result
		}
		
		val prevIncreaseSeq = (1 to popSize/2)
		
		prevIncreaseSeq.map(i => (i,i))
			.view
			.map{case (inc, jnc) => (confidenceInDetection(inc), jnc)}
			.find(_._1 > reqConfidence)
			.map(_._2)
	}
	
	// Choose a sample size M
	val secondSampleSize = (10 to 60) by 1// map (i => 5*i + 20)
	val minDetectableOption = secondSampleSize.map(smallestDetectablePrevIncrease)
	
	val power = (secondSampleSize zip minDetectableOption)
			.filter{case (ss, md) => md.isDefined}.map{case (ss, md) => (ss,md.get)}
	
	import sampler.data.Types._
	new CSVTableWriter(Paths.get("PrevChange.csv"))(
		Column(power.map(_._1), "SampleSize"),
		Column(power.map(_._2), "minDetectable")
	)
	
	// Plot a graph in R
	val script = """
require(ggplot2)
require(plyr)
require(reshape)
		
values = read.csv("PrevChange.csv")
pdf("graph.pdf", width=8.27, height=5.83)
ggplot(values, aes(x=SampleSize, y=minDetectable)) + geom_line()
dev.off()
"""
	ScriptRunner(script, workDir.resolve("script.r"))
}