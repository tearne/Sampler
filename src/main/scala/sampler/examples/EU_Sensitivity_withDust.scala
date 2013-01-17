/*
 * Copyright (c) 2013 Crown Copyright 
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

import sampler.data.EmpiricalMetricComponent
import java.nio.file.Paths
import sampler.io.ChainReader
import sampler.math.Random
import sampler.math.Statistics
import scala.annotation.tailrec
import scala.collection.GenSeq
import sampler.math.Probability
import sampler.data.Samplable
import sampler.data.Empirical._
import sampler.data.ParallelSampleBuilder
import sampler.data.Empirical

object EU_Sensitivity_withDust extends App with EmpiricalMetricComponent{
  
    val pathspec = Paths.get("", "examples", "ncpSampleSize", "data", "coda")
  
  val chains = ChainReader(pathspec.toString())

//  NCP cage
  
  val populationNames = List(
	"PPosEUFaecesCage[1]",
	"PPosEUFaecesCage[2]",
	"PPosEUFaecesCage[3]",
	"PPosEUFaecesCage[4]",
	"PPosEUFaecesCage[5]",
	"PPosEUFaecesCage[6]",
	"PPosEUFaecesNonCage[1]",
	"PPosEUFaecesNonCage[2]",
	"PPosEUFaecesNonCage[3]",
	"PPosEUFaecesNonCage[4]",
	"PPosEUFaecesNonCage[5]",
	"PPosEUFaecesNonCage[6]"
  )
  
  val dustNames = List(
	"PPosEUDustCage[1]",
	"PPosEUDustCage[2]",
	"PPosEUDustCage[3]",
	"PPosEUDustCage[4]",
	"PPosEUDustCage[5]",
	"PPosEUDustCage[6]",
	"PPosEUDustNonCage[1]",
	"PPosEUDustNonCage[2]",
	"PPosEUDustNonCage[3]",
	"PPosEUDustNonCage[4]",
	"PPosEUDustNonCage[5]",
	"PPosEUDustNonCage[6]"
  )

  // Preamble
  implicit val r = new Random()
  val stats = new Statistics
  
  // Metaparameters
  val requiredConf = 0.95
  val chunkSize = 2000

  // Run analysis
  val faecesChains = populationNames 
		  .map (name => chains(name))
		  .map (list => list.toEmpiricalSeq)
		  
  val dustChains = dustNames 
		  .map (name => chains(name))
		  .map (list => list.toEmpiricalSeq)
  
  val sampleChains = faecesChains zip dustChains

  case class Sensitivity(faeces: Double, dust: Double)
  def combinedDist(fDist: Samplable[Double, Random], dDist: Samplable[Double, Random]) = new Samplable[Sensitivity, Random]{
    
    def sample(implicit r: Random) = {
      Sensitivity(fDist.sample(r), dDist.sample(r))
    }
  }
  
  val faecesSS = 5
  val dustSS = 2
    
  val samplableChains = sampleChains.map(faeces => combinedDist(faeces._1, faeces._2))
  
  val sampleSizes = samplableChains.map (chains => calculateConfidence(chains, faecesSS, dustSS))
  
  // Report
  populationNames zip sampleSizes foreach(println)
  
  // Analysis code
  def calculateConfidence(sensitivityDist: Samplable[Sensitivity, Random], numFaecal: Int, numDust: Int) = {
      // Single trial Se => Multiple trials Se
      def probDetection(se: Sensitivity) = 1 - math.pow((1 - se.faeces), numFaecal) * math.pow((1 - se.dust), numDust)
     
      // Termination condition for drawing samples
      def terminateCondition(soFar: GenSeq[Boolean]) = {
    	val distance = metric.max(
    	    soFar.toEmpiricalTable, 
    	    soFar.take(soFar.size - chunkSize).toEmpiricalTable
    	)
    	
    	(distance < 0.0001) || (soFar.size > 1e8)
      }
      
      // The proportion of samples so far that are positive
      def proportionPositive(samples: GenSeq[Boolean]) = 
        samples.toEmpiricalTable.probabilities(true).value
      
      // Build model for result of 'numTrials',
      //incorporating uncertainty in test performance
      val detectionProbs = sensitivityDist map (se => Probability(probDetection(se)))
      val model = Samplable.bernouliTrial(detectionProbs)
      
      // Build sampling distribution and finish
      //if confidence in +ve result suff high 
      val samples = new ParallelSampleBuilder(chunkSize)(model)(terminateCondition)
      
      // return proportion positive (i.e. confidence)
      proportionPositive(samples)
  }

}