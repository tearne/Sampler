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

import sampler.data.Empirical._
import sampler.math.Random
import sampler.data.Samplable
import scala.collection.parallel.ParSeq
import java.nio.file.{Paths, Files}
import sampler.io.CSVTableWriter
import sampler.data.Types.Column
import sampler.math.Probability
import sampler.r.ScriptRunner
import scala.annotation.tailrec
import sampler.data.EmpiricalMetricComponent
import sampler.data.EmpiricalTable
import sampler.data.ParallelSampleBuilder
import scala.collection.GenSeq

object SampleDistribution extends App with EmpiricalMetricComponent{
	/*
	 * In a population of a given size, sampling a given number 
	 * without replacement, what is the distribution of results 
	 * seen from the sampling
	 */
 	//Domain parameters
	val populationSize = 100
	val sampleSize = 35
	
	val chunkSize = 2000
	val convergenceTolerance = 0.01
	implicit val random = new Random
	val outputPath = Paths.get("examples", "sampleSize")
	Files.createDirectories(outputPath)
	
	val prevalenceOfInterest = List(5, 10, 20, 30, 50, 70, 90)
	
	def numPositivesDistribution(numPositive: Int, populationSize: Int, sampleSize: Int): Samplable[Int, Random] = {
		val population = (1 to populationSize).map(_ <= numPositive)
		val model = Samplable.withoutReplacement(population, sampleSize)// Start with base model
				.map(_.count(identity))// / sampleSize.toDouble)		// Transform to model of sample prevalance
		model
	}
	
	def terminationCondition(soFar: GenSeq[Int]) = {
		val distance = metric.max(
			soFar.take(soFar.size - chunkSize).toEmpiricalTable, 
			soFar.toEmpiricalTable
		)
		  
		(distance < convergenceTolerance) || (soFar.size > 1e16)
	  }
//	
//	val samplingDistributions: Map[Int, EmpiricalTable[Int]] = prevalenceOfInterest.map{trueNumPositives => 
//	  	println(trueNumPositives)
//		
//		val model = numPositivesDistribution(trueNumPositives, populationSize, sampleSize)
//	  
//	  	// Sample the model until convergence
//	  	val builder = new ParallelSampleBuilder(chunkSize)
//	  	val dist = builder(model)(terminationCondition _).toEmpiricalTable
//	  	(trueNumPositives, dist)
//	}.toMap
//	
//	
//	val potentialPrevObservations =  (0 to populationSize).map(_.toDouble / populationSize)
//	val dataColumns = prevalenceOfInterest.map{p =>
//		@tailrec
//		def addZeroIfMissing(map: Map[Int, Double], keyRange: Seq[Int]): Map[Int, Double] = {
//			if(keyRange.size == 0) map
//			else{
//				val newMap = if(!map.contains(keyRange.head)) map + (keyRange.head -> 0.0) else map
//				addZeroIfMissing(newMap, keyRange.tail)
//			}
//		}
//		
//		val dist = addZeroIfMissing(
//				samplingDistributions(p).probabilities.mapValues(_.value),
//				0 to populationSize
//		).mapValues(_.toDouble / sampleSize)
//		
//		Column(
//			(0 to sampleSize).map(possiblePositives => dist(possiblePositives)), 
//			p.toString
//		)
//	}
//	val t = Column((0 to sampleSize).map(_.toDouble / sampleSize), "ObsPrev")
//	val a = t :: dataColumns
//	
//	new CSVTableWriter(outputPath.resolve("output.csv"), true)(a: _*)
//	
//	val plotScript = """
//require("ggplot2")
//require("reshape")
//
//data <- read.csv("output.csv")
//
//names(data) <- c("ObsPrev", "5", "10", "20", "30", "50", "70", "90")
//
//pdf("samplingDists.pdf", width=4.13, height=2.91) #A7 landscape paper)
//ggplot(melt(data,id="ObsPrev"), aes(x=ObsPrev, y=value, colour=variable)) +  
//    geom_line() + 
//    scale_x_continuous(name="Observed Prevalence") +
//    scale_y_continuous(name="Density") + 
//    scale_colour_discrete(name="True Prev (%)") + 
//	opts(title = "title")
//dev.off()
//"""
//
//	ScriptRunner(plotScript, outputPath.resolve("plotScript.r"))

}
