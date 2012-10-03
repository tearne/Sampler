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
import sampler.data.Empirical._
import scala.annotation.tailrec
import sampler.data.Samplable
import sampler.fit.Prior
import sampler.io.CSVTableWriter
import java.nio.file.Paths
import sampler.data.Types.Column
import sampler.math._
import sampler.r.ScriptRunner
import sampler.run.agent.LocalActorRunner
import sampler.data.Types._
import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import sampler.fit.ABCComponent
import sampler.data._
import sampler.data.EmpiricalMetricComponent

/*
 * On first sample of an infected population work out the uncertainty around
 * the sample prevalence.  Then, supposing the prevalence subsequently increases
 * by alpha (absolute number of infected animals), work out the minimum number
 * of samples to have 95% confidence of detecting an increase, given all the
 * uncertainty thus far.
 */
object PrevChangeApp extends App 
		with WithoutReplacementABC 
		with EmpiricalMetricComponent
	//	with StatisticsComponent
		with Environment{
	
	val populationSize = 60
	val sampleSize = 20
	val numPositiveObservations = 7
	
	val numParticles = 10000

	val model = new Model(populationSize, sampleSize)
	import model._
	
		
	object ABC extends ABCComponent 
				  with SampleBuilderComponent{
		val builder = SerialSampleBuilder
	}
	
	//Get a posterior for the true number of infected given sample so far
	def getPosterior(numPosObserved: Int) = {
		val runner = new LocalActorRunner()
		
		val posterior = ABC.apply(model, r)(
				uniformPrior,
				Observations(numPosObserved),
				reps = 10,
				particles = numParticles,
				startTolerance = 10,
				refinementAttempts = 6,
				runner,
				None
		)
		
		runner.shutdown
		posterior
	}
	val posterior = getPosterior(numPositiveObservations)
	
	// Plot the posterior
	val counts = posterior.probabilities.map{case (k,v) => k.numInfected -> v}
	@tailrec
	def addZeroIfMissing(map: Map[Int, Probability], keyRange: Seq[Int]): Map[Int, Probability] = {
		if(keyRange.size == 0) map
		else{
			val newMap = if(!map.contains(keyRange.head)) map + (keyRange.head -> Probability.zero) else map
			addZeroIfMissing(newMap, keyRange.tail)
		}
	}
	val plotData = TreeMap(addZeroIfMissing(counts, 0 to populationSize).toSeq: _*).toSeq
	
	new CSVTableWriter(workingDir.resolve("posterior.csv"), true)(
		Column(plotData.map(_._1), "TruePositives"),
		Column(plotData.map(_._2), "RelFreq")
	)
	
	val posteriorPlotScript = 
"""
require(ggplot2)
require(reshape)
			
posteriors = read.csv("posterior.csv")
			
pdf("posterior.pdf", width=8.27, height=5.83)
ggplot(melt(posteriors, id="TruePositives"), aes(x=TruePositives, y=value, colour=variable)) +
	geom_line(adjust=1) +
	scale_x_continuous(name="True Num Positives")
dev.off()
"""
	ScriptRunner(posteriorPlotScript, workingDir.resolve("posterior.r"))
	
	//Work out what power a new sample would have to identify a 
	//prevalence change with the desired  confidence
	val requiredConfidence = Probability(0.80)
	val mcChunkSize = 100
	val mcConvergence = 0.001
	
	def smallestDetectablePrevIncrease(newSampleSize: Int): Option[Double] = {
		def confidenceInDetection(extraNumInf: Int): Probability = {
			val restrictedPosterior = posterior
					.filter(_.numInfected < populationSize - extraNumInf)
					.map((p: Parameters) => p.numInfected)
					
			val differenceDist = new Samplable[Int]{
				def sample(implicit r: Random) = {
					val numInfectedPast = restrictedPosterior.sample
					val numInfectedPresent = numInfectedPast + extraNumInf
					modelDistribution(numInfectedPresent, newSampleSize, populationSize).sample - numInfectedPast
				}
			} 
					
			val builder = new ParallelSampleBuilder(mcChunkSize)
			val result = builder(differenceDist){samples => 
				val distance = metric.max(
						samples.take(samples.size - mcChunkSize).toEmpiricalTable, 
						samples.toEmpiricalTable
				)
				(distance < mcConvergence) || (samples.size > 1e8)
			}.toEmpiricalTable.rightTail(0)
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
	val secondSampleSize = (20 to 60)
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