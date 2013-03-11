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
import java.nio.file.Paths
import sampler.io.CSVTableWriter
import sampler.data.Types.Column
import sampler.math.Probability
import sampler.math.StatisticsComponentImpl
import sampler.r.ScriptRunner
import scala.annotation.tailrec
import sampler.data.EmpiricalMetricComponentImpl
import sampler.data.EmpiricalTable
import sampler.data.ParallelSampleBuilder
import scala.collection.GenSeq

object SampleDistribution extends App with EmpiricalMetricComponentImpl with StatisticsComponentImpl {
	/*
	 * In a population of a given size, sampling a given number 
	 * without replacement, what is the distribution of results 
	 * seen from the sampling
	 */
  
  //Domain parameters
  val populationSize = 100
  val sampleSize = 10
  
  //Meta-parameters
  val chunkSize = 2000
  val convergenceCriterion = 0.001
  
  implicit val random = new Random
  
  val outputPath = Paths.get("examples", "sampleDists")
  
  val listOfTestPrevs = List(5, 10, 20, 30, 50, 70, 90)
  
  var listOfPosiblePositive: List[Int] = List()
  var listOfSamplePrevs: List[Double] = List()
  
  for(i <- 0 to sampleSize) {
	  listOfPosiblePositive = listOfPosiblePositive.:+(i)
	  listOfSamplePrevs = listOfSamplePrevs.:+(i.toDouble/sampleSize * 100)
  }
  
  var columns = List(
      Column(
    	listOfPosiblePositive.map(a => a.toDouble/sampleSize * 100), 
    	"ObsPrev"
  ))
	  
  for(prev <- listOfTestPrevs) {
    // Potential for refactoring into a 'getDistributionColumn' prevalence
    val sampleDist = 
      sampleDistribution(prev.toDouble / 100.0, populationSize, sampleSize)
      .probabilities
    
    val numPosDist = sampleDist map {
      case a => (a._1 * sampleSize).toInt -> a._2.value
    }
    
    val completeDist = addZeroIfMissing(numPosDist, (0 to sampleSize))
    
    val sampleProbs = listOfPosiblePositive.map(a => completeDist.getOrElse(a, 0.0)).toList

    columns = columns.:+(Column(sampleProbs, prev.toString))
  }
  
  val scenario = "Pop" + populationSize + "Sample" + sampleSize
  val longScenario = "Population Size = " + populationSize + ", Sample Size = " + sampleSize
  val csvFileName = scenario + ".csv"
  val pdfFileName = scenario + ".pdf"
  
  new CSVTableWriter(outputPath.resolve(csvFileName), true)(columns: _*)
  
  val plotScript = """
require("ggplot2")
require("reshape")

data <- read.csv("""" + csvFileName + """")

names(data) <- c("ObsPrev", "5", "10", "20", "30", "50", "70", "90")

pdf("""" + pdfFileName + """", width=8.27, height=5.83)
ggplot(melt(data,id="ObsPrev"), aes(x=ObsPrev, y=value, colour=variable)) +  
    geom_line() + 
    scale_x_continuous(name="Observed Prevalence") +
    scale_y_continuous(name="Density") + 
    scale_colour_discrete(name="True Prev (%)") + 
	opts(title = """" + longScenario + """")
dev.off()
"""

  ScriptRunner(plotScript, outputPath.resolve("plotScript.r"))

  /** Takes a long time to run so commented out for later runs **/
//  val confidence = Probability(0.95)
//  val precision = 0.09
//  
//  var sampleSizes: List[Int] = List()
//  var prevalences: List[Int] = List()
//  
//  for(i <- 1 until 100) {
//	  prevalences = prevalences.:+(i)
//	  
//	  println("Calculating sample size for prevalence " + i + "% ...")
//	  
//	  val n = minimumSampleSize(populationSize, i.toDouble/100.0, confidence, precision)
//	  sampleSizes = sampleSizes.:+(n)
//	  
//	  println("\t" + n + " samples")
//  }
//  
//  val prevCol = Column(prevalences, "Prevalence")
//  val ssCol = Column(sampleSizes, "SampleSize")
//  
//  new CSVTableWriter(outputPath.resolve("SSfor100with95.csv"), true)(prevCol, ssCol)
  
  val ssScript = """
require("ggplot2")
require("reshape")
    
data <- read.csv("SSfor100with95.csv")

formulaN = c()

for(i in 1:99){
prev = i/100
formulaN = c(formulaN, (1.96^2 * prev * (1-prev))/0.09^2)
}
    
newData <- c(data, as.data.frame(formulaN))
newData <- as.data.frame(newData)

names(newData) <- c("Prevalence", "Sampling", "Classical Stats")

pdf("ssPlot.pdf", width=8.27, height=5.83)
    
ggplot(melt(newData,id="Prevalence"), aes(x=Prevalence, y=value, colour=variable)) + 
    geom_line() + 
    scale_x_continuous(name="Prevalence") +
    scale_y_continuous(name="Sample size") +
    scale_colour_discrete(name="Method")

reduced2 <- read.csv("SSfor100with95_reduced2.csv")

reducedN = c()
    
for(i in seq(2, 98, 2)){
prev = i/100
reducedN = c(reducedN, (1.96^2 * prev * (1-prev))/0.09^2)
}

reducedData <- c(reduced2, as.data.frame(reducedN))
reducedData <- as.data.frame(reducedData)

names(reducedData) <- c("Prevalence", "Sampling", "Classical Stats")
    
ggplot(melt(reducedData,id="Prevalence"), aes(x=Prevalence, y=value, colour=variable)) + 
    geom_line() + 
    scale_x_continuous(name="Prevalence") +
    scale_y_continuous(name="Sample size") +
    scale_colour_discrete(name="Method")
  
dev.off()
"""

  ScriptRunner(ssScript, outputPath.resolve("ssScript.r"))
  
  def sampleDistribution(truePrevalence: Double, populationSize: Int, sampleSize: Int): EmpiricalTable[Double] = {
		  
	  val numInfected = (populationSize*truePrevalence).round.toInt
	  val population = (1 to populationSize).map(_ <= numInfected)
	  
	  val model = 
	  	Samplable.withoutReplacement(population, sampleSize)	// Start with base model
	  	.map(_.count(identity) / sampleSize.toDouble)			// Transform to model of sample prevalance
	  
	  def terminationCondition(soFar: GenSeq[Double]) = {
		val distance = max(
			soFar.take(soFar.size - chunkSize).toEmpiricalTable, 
			soFar.toEmpiricalTable
		)
		  
		(distance < convergenceCriterion) || (soFar.size > 1e8)
	  }
		  
	  // Sample the model until convergence
	  val builder = new ParallelSampleBuilder(chunkSize)
	  builder(model)(terminationCondition _).toEmpiricalTable
  }
  
  @tailrec
	def addZeroIfMissing(map: Map[Int, Double], keyRange: Seq[Int]): Map[Int, Double] = {
		if(keyRange.size == 0) map
		else{
			val newMap = if(!map.contains(keyRange.head)) map + (keyRange.head -> 0.0) else map
			addZeroIfMissing(newMap, keyRange.tail)
		}
	}
  
  def sampleSizeConfidence(sampleDist: EmpiricalTable[Double], precision: Double, truePrev: Double) = {
	  sampleDist.counts
    	.map{case (k,v) => (math.abs(k - truePrev) < precision) -> v}
    	.toEmpiricalTable
  }
  
  def minimumSampleSize(popSize: Int, truePrev: Double, requiredConfidence: Probability, precision: Double) = {
	  val result = {
	    (1 to popSize)
	    	.view
	    	.map { n =>
	    	  val eo = sampleSizeConfidence(
	    	      sampleDistribution(truePrev, popSize, n), 
	    	      precision,
	    	      truePrev
	    	  )
	    	  val confidence = eo.probabilities.getOrElse(true, Probability.zero)
//	          println("Sample size = %d, empirical size = %d, confidence = %s".format(n, eo.size, confidence.toString))
	          (n, confidence)
			}
			.find(_._2 > requiredConfidence)
			.get._1
	  }
	  result
  }
}
