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

import sampler.data.Empirical._
import sampler.io.CSVTableWriter
import sampler.r.ScriptRunner
import sampler.data.Types._
import sampler.math._
import sampler.data._
import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import sampler.run.actor.LocalActorRunner
import sampler.fit.ABCComponent
import sampler.run.SerialRunner
import sampler.fit.ABCParameters

/*
 * Draw posterior distributions and confidence limits for the number of infected 
 * units in a population of size 60, assuming a uniform prior and samples (without 
 * replacement) of size 20
 */
object Posteriors extends App with WithoutReplacementABC with Environment{
	val populationSize = 60
	val sampleSize = 20
	val numPositiveObservations = 3
	
	val model = new Model(populationSize, sampleSize)
	import model._
	
	object ABC extends ABCComponent 
				  with SampleBuilderComponent{
		val builder = SerialSampleBuilder
	}
	
	def getPosteriorNumInfected(numPosObserved: Int): EmpiricalTable[Int] = {
		val runner = new LocalActorRunner()
		
		val posterior = ABC(model, r)(
				uniformPrior,
				Observations(numPosObserved),
				new ABCParameters(10, 10000, 10, 8, 500),
				runner,
				None
		)
		
		runner.shutdown
		posterior.map{_.numInfected}.toEmpiricalTable
	}
	
//	val potentialObservations = List(0,1,3,10,17,19,20)
	val potentialObservations = List(0,20)
	val posteriors = potentialObservations.map{i => getPosteriorNumInfected(i)}
	
	@tailrec
	def addZeroIfMissing(map: Map[Int, Int], keyRange: Seq[Int]): Map[Int, Int] = {
		if(keyRange.size == 0) map
		else{
			val newMap = if(!map.contains(keyRange.head)) map + (keyRange.head -> 0) else map
			addZeroIfMissing(newMap, keyRange.tail)
		}
	}
	
	val posteriorCounts = posteriors
			.map{_.counts}
			.map(counts => addZeroIfMissing(counts, 0 to populationSize))
			.map(counts => TreeMap(counts.toSeq: _*))
			.map(_.iterator.map(_._2).toList)
			
			
	val columns = Column(0 to populationSize, "TruePositives") +: 
		posteriorCounts.zip(potentialObservations).map{case (counts, observedPos) =>
			Column(counts.toSeq, observedPos.toString)
		}
	
	new CSVTableWriter(workingDir.resolve("posteriors.csv"), true)(
		columns: _*
	)

	val posteriorPlotScript = 
"""
require(ggplot2)
require(reshape)
			
posteriors = read.csv("posteriors.csv")
			
pdf("posteriors.pdf", width=8.27, height=5.83)
ggplot(melt(posteriors, id="TruePositives"), aes(x=TruePositives, y=value, colour=variable)) +
	geom_line(adjust=1) +
	scale_x_continuous(name="True Num Positives") +
	scale_y_continuous(name="Density") +
	scale_colour_discrete(name="Num Positive Obs.")
dev.off()
"""
	ScriptRunner(posteriorPlotScript, workingDir.resolve("posteriors.r"))
	
	// Plot some confidence limits versus potential observations
	case class Confidence(lower: Double, upper: Double)
	def getConfidence(posterior: EmpiricalTable[Int]) = {
		val p = posterior.counts.map{case (k,v) => k.toDouble -> v}.toEmpiricalTable
		val left = p.quantile(Probability(0.025))
		val right = p.quantile(Probability(0.975))
		Confidence(left, right)
	}
	
	val results = posteriors.map(getConfidence)
	
	new CSVTableWriter(workingDir.resolve("confidence.csv"), true)(
		Column(potentialObservations, "numPositiveObserved"),
		Column(results.map(_.lower), "lower"),
		Column(results.map(_.upper), "upper")
	)
	
	//Plot a graph in R
	val confidencePlotScript = 
"""
require(ggplot2)
require(reshape)
			
values = read.csv("confidence.csv")
pdf("confidence.pdf", width=8.27, height=5.83)
ggplot(melt(values, id="numPositiveObserved"), aes(x=numPositiveObserved, colour=variable, y=value)) + 
	geom_line() +
	scale_x_continuous(name="Num Positives Observed") +
	scale_y_continuous(name="True Num Positives") +
	scale_colour_discrete(name="95% Confidence Limits")
dev.off()
"""
	ScriptRunner(confidencePlotScript, workingDir.resolve("confidence.r"))
	
}

	