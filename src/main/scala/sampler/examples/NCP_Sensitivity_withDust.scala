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

object NCP_Sensitivity_withDust extends App with EmpiricalMetricComponent {

  val pathspec = Paths.get("", "examples", "ncpSampleSize", "data", "coda")
  
  val chains = ChainReader(pathspec.toString())

//  NCP cage
  
  val populationNames = List(
	"PPosNCPFaecesCage[1]",
	"PPosNCPFaecesCage[2]",
	"PPosNCPFaecesCage[3]",
	"PPosNCPFaecesCage[4]",
	"PPosNCPFaecesCage[5]",
	"PPosNCPFaecesCage[6]"
  )
  
  val dustNames = List(
	"PPosNCPDustCage[1]",
	"PPosNCPDustCage[2]",
	"PPosNCPDustCage[3]",
	"PPosNCPDustCage[4]",
	"PPosNCPDustCage[5]",
	"PPosNCPDustCage[6]"
  )

//  NCP non cage
  
//  val populationNames = List(
//	"PPosNCPFaecesNonCage[1]",
//	"PPosNCPFaecesNonCage[2]",
//	"PPosNCPFaecesNonCage[3]",
//	"PPosNCPFaecesNonCage[4]",
//	"PPosNCPFaecesNonCage[5]",
//	"PPosNCPFaecesNonCage[6]"
//  )
//  
//  val dustNames = List(
//	"PPosNCPDustNonCage[1]",
//	"PPosNCPDustNonCage[2]",
//	"PPosNCPDustNonCage[3]",
//	"PPosNCPDustNonCage[4]",
//	"PPosNCPDustNonCage[5]",
//	"PPosNCPDustNonCage[6]"
//  )

  // Preamble
  implicit val r = new Random()
  val stats = new Statistics
  
  // Metaparameters
  val requiredConf = 0.95
  val chunkSize = 2000

  // Run analysis
  val faecesChains = populationNames 
		  .map (name => chains(name))
		  
  val dustChains = populationNames 
		  .map (name => chains(name))
  
  val sampleChains = faecesChains zip dustChains
		  
  val minimumSampleSizes = sampleChains.map (chains => smallestSampleSize(chains._1, chains._2))
  
  // Report
  populationNames zip minimumSampleSizes foreach(println)
  		  
  // Analysis code
  def smallestSampleSize(faecesDist: Seq[Double], dustDist: Seq[Double]) = {
    @tailrec
    def calcConf(numTrials: Int) : Int = {
      // Single trial Se => Multiple trials Se
      def probDetection(p: Double, sampleSize: Int) = 1 - math.pow((1 - p), sampleSize)
     
      // Termination condition for drawing samples
      def terminateCondition(soFar: GenSeq[Boolean]) = {
    	val distance = metric.max(
    	    soFar.toEmpiricalTable, 
    	    soFar.take(soFar.size - chunkSize).toEmpiricalTable
    	)
    	(distance < 0.0001) || (soFar.size > 1e8)
      }
      
      // The proportion of samples so far that are positive
      def proportionPositive(faeces: GenSeq[Boolean], dust: GenSeq[Boolean]) = {
        val pFaeces = faeces.toEmpiricalTable.probabilities(true).value
		val pDust = dust.toEmpiricalTable.probabilities(true).value
		
		1 - ((1 - pFaeces) * (1 - pDust))
      }
      
      // Build model for result of 'numTrials',
      //incorporating uncertainty in test performance
      val faecesDetectionProbs = faecesDist map (se => Probability(probDetection(se, numTrials)))
      val dustDetectionProbs = dustDist map (se => Probability(probDetection(se, 2)))
      
      val faecesModel = Samplable.bernouliTrial(faecesDetectionProbs.toEmpiricalSeq)
      val dustModel = Samplable.bernouliTrial(dustDetectionProbs.toEmpiricalSeq)
      
      // Build sampling distribution and finish
      //if confidence in +ve result suff high 
      val faecesSamples = new ParallelSampleBuilder(chunkSize)(faecesModel)(terminateCondition)
      val dustSamples = new ParallelSampleBuilder(chunkSize)(dustModel)(terminateCondition)

      if(proportionPositive(faecesSamples, dustSamples) > requiredConf) numTrials
      else calcConf(numTrials + 1)  
    }
    
    calcConf(1)
  }
}