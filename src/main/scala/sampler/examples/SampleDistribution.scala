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
import sampler.data.Samplable
import scala.collection.parallel.ParSeq
import sampler.data.Distance
import sampler.data.FrequencyTableBuilder
import sampler.data.FrequencyTable
import java.nio.file.Paths
import sampler.io.CSVTableWriter
import sampler.data.Types.Column
import sampler.math.Probability
import sampler.r.ScriptRunner

object SampleDistribution extends App {
	/*
	 * In a population of a given size, sampling a given number 
	 * without replacement, what is the distribution of results 
	 * seen from the sampling
	 */
  
  //Domain parameters
  val populationSize = 100
  val sampleSize = 10
  val precision = 0.1
  
  //Meta-parameters
  val chunkSize = 2000
  val convergenceCriterion = 0.001
  
  implicit val random = new Random
  
  val outputPath = Paths.get("examples", "sampleDists")
  
  val listOfTestPrevs = List(5, 10, 20, 30, 50, 70, 90)
  
  var listOfSamplePrevs: List[Double] = List()
  
  for(i <- 0 to sampleSize)
	  listOfSamplePrevs = listOfSamplePrevs.:+(i/sampleSize.toDouble * 100)
  
  var columns = List(Column(listOfSamplePrevs, "ObsPrev"))
	  
  for(prev <- listOfTestPrevs) {
    val sampleDist = 
      sampleDistribution(prev.toDouble / 100.0)
      .probabilityMap
    
    val sampleProbs = listOfSamplePrevs.map(a => sampleDist.getOrElse(a.toDouble / 100, Probability(0)).value)

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

data = read.csv("""" + csvFileName + """")

pdf("""" + pdfFileName + """", width=8.27, height=5.83)
ggplot(melt(data,id="ObsPrev"), aes(x=ObsPrev, y=value, colour=variable)) +  
    geom_line() + 
    scale_x_continuous(name="Observed Prevalence") +
    scale_y_continuous(name="Density") + 
    scale_colour_discrete(name="True Prev (%)") + 
	opts(title = """" + longScenario + """")
dev.off()
"""

  ScriptRunner(plotScript, outputPath.resolve("plorScript.r"))
    
  def sampleDistribution(truePrevalence: Double): FrequencyTable[Double] = {
		  
	  val numInfected = (populationSize*truePrevalence).round.toInt
	  val population = (1 to populationSize).map(_ <= numInfected)
	  
	  val model = 
	  	Samplable.withoutReplacement(population, sampleSize)	// Start with base model
	  	.map(_.count(identity) / sampleSize.toDouble)			// Transform to model of sample prevalance
	  
	  def terminationCondition(soFar: ParSeq[Double]) = {
		val distance = Distance.max(
			FrequencyTable(soFar.seq.take(soFar.size - chunkSize)), 
			FrequencyTable(soFar.seq)
		)
		  
		(distance < convergenceCriterion) || (soFar.size > 1e8)
	  }
		  
	  // Sample the model until convergence
	  FrequencyTableBuilder.parallel(model, chunkSize)(s => terminationCondition(s))
  }
}