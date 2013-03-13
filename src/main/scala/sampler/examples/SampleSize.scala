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

import sampler.data._
import sampler.data.Empirical._
import sampler.math.{Random, Probability}
import scala.collection.GenSeq
import sampler.data.Types.Column
import scala.annotation.tailrec
import sampler.io.CSVTableWriter
import sampler.r.ScriptRunner
import java.nio.file.{Paths, Files}
import sampler.math.StatisticsComponent
import sampler.math.StatisticsComponentImpl

object SampleSize extends App with EmpiricalMetricComponentImpl with StatisticsComponentImpl{
	//Domain parameters
	val populationSize = 100
	val sampleSize = 35
	
	val chunkSize = 2000
	val convergenceTolerance = 0.01
	implicit val random = new Random

	val wd = Paths.get("egoutput","sampleSize")
	Files.createDirectories(wd)
	
	def numPositivesDistribution(numPositive: Int, populationSize: Int, sampleSize: Int): Samplable[Int] = {
		val population = (1 to populationSize).map(_ <= numPositive)
		val model = Samplable.withoutReplacement(population, sampleSize)// Start with base model
				.map(_.count(identity))// / sampleSize.toDouble)		// Transform to model of sample prevalance
		model
	}
	
	def terminationCondition(soFar: GenSeq[Int]) = {
		val distance = max(
			soFar.take(soFar.size - chunkSize).toEmpiricalTable, 
			soFar.toEmpiricalTable
		)
		  
		(distance < convergenceTolerance) || (soFar.size > 1e16)
	}
	
	//
	// Now how many samples to be 95% confident in hitting true prev 
	//
	
	val confidence = Probability(0.95)
	val precision = 0.1
	val biggestPossibleSample = populationSize
	
	def simulatedSampleSize(truNumPos: Int): Int = {
		println(s"Working on $truNumPos")
		
		def samplingDistribution(trueNumPos: Int, popSize: Int, sampSize: Int) = {
			val model = numPositivesDistribution(trueNumPos, popSize, sampSize)
			
		  	// Sample the model until convergence
		  	val builder = new ParallelSampleBuilder(chunkSize)
		  	val dist = builder(model)(terminationCondition _).toEmpiricalTable
		  	val obsDist = dist.counts.map{case (k,v) => (k.toDouble / sampSize, v)}.toEmpiricalTable
		  	obsDist
		}
		
		val reqSampleSize = (1 to biggestPossibleSample)
			.view
			.map{sampleSize =>
				//println("trying "+sampleSize)
				val stats = new StatisticsComponentImpl(){}
				val sampDist = samplingDistribution(truNumPos, populationSize, sampleSize)
				val lowerTail = stats.quantile(sampDist, Probability(0.025))
				val upperTail = stats.quantile(sampDist, Probability(0.975))
				val truePrev = truNumPos.toDouble / populationSize
				val lowerAcceptibleError = truePrev - precision
				val upperAcceptibleError = truePrev + precision
				//println(s"$lowerAcceptibleError, $lowerTail, $upperTail, $upperAcceptibleError")
				val acceptibleConfidence = lowerAcceptibleError <= lowerTail && upperTail <= upperAcceptibleError 
				(sampleSize, acceptibleConfidence)
			}
			.find(_._2)
			.get._1
		
		println(s"Sample size is $reqSampleSize")
		reqSampleSize
	}
	
	//From 
	//http://www.raosoft.com/samplesize.html
	// & 
	//http://uregina.ca/~morrisev/Sociology/Sampling%20from%20small%20populations.htm
	
	def binomialSampleSize(trueNumPos: Int) = {
		val prev = trueNumPos.toDouble / populationSize
		math.pow(1.96,2) * prev * (1 - prev) / math.pow(precision,2)
	}
	
	def hypergeometricSampleSize(trueNumPos: Int) = {
		val prev = trueNumPos.toDouble / populationSize
		val x = math.pow(1.96,2) * prev * (1-prev)
		(populationSize * x)/(math.pow(precision,2)*(populationSize - 1) + x)
	}
	
	val possibleTruePositives = 1 to 99
	
	new CSVTableWriter(wd.resolve("sampleSizes.csv"), true)(
		Column(possibleTruePositives, "TruePos"),
		Column(possibleTruePositives.map(simulatedSampleSize), "Sampled"),
		Column(possibleTruePositives.map(binomialSampleSize), "Binomial"),
		Column(possibleTruePositives.map(hypergeometricSampleSize), "Hyperg")
	)
	
	val sampleSizeScript = """
require("ggplot2")
require("reshape")

data <- read.csv("sampleSizes.csv")

#names(data) <- c("ObsPrev", "5", "10", "20", "30", "50", "70", "90")

pdf("sampleSize.pdf", width=4.13, height=2.91) #A7 landscape paper
ggplot(melt(data,id="TruePos"), aes(x=TruePos, y=value, colour=variable)) +  
    geom_line() + 
#    scale_x_continuous(name="Observed Prevalence") +
#    scale_y_continuous(name="Density") + 
#    scale_colour_discrete(name="True Prev (%)") + 
	opts(title = "title")
dev.off()
"""
		
		ScriptRunner(sampleSizeScript, wd.resolve("sampleSizeScript.r"))
}