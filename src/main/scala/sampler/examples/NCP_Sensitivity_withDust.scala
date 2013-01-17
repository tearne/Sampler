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

object NCP_Sensitivity_withDust extends App with EmpiricalMetricComponent {

  val pathspec = Paths.get("", "examples", "ncpSampleSize", "data", "coda")
  
  val chains = ChainReader(pathspec.toString())

//  NCP cage
  
//  val populationNames = List(
//	"PPosNCPFaecesCage[1]",
//	"PPosNCPFaecesCage[2]",
//	"PPosNCPFaecesCage[3]",
//	"PPosNCPFaecesCage[4]",
//	"PPosNCPFaecesCage[5]",
//	"PPosNCPFaecesCage[6]"
//  )
//  
//  val dustNames = List(
//	"PPosNCPDustCage[1]",
//	"PPosNCPDustCage[2]",
//	"PPosNCPDustCage[3]",
//	"PPosNCPDustCage[4]",
//	"PPosNCPDustCage[5]",
//	"PPosNCPDustCage[6]"
//  )

//  NCP non cage
  
  val populationNames = List(
	"PPosNCPFaecesNonCage[1]",
	"PPosNCPFaecesNonCage[2]",
	"PPosNCPFaecesNonCage[3]",
	"PPosNCPFaecesNonCage[4]",
	"PPosNCPFaecesNonCage[5]",
	"PPosNCPFaecesNonCage[6]"
  )
  
  val dustNames = List(
	"PPosNCPDustNonCage[1]",
	"PPosNCPDustNonCage[2]",
	"PPosNCPDustNonCage[3]",
	"PPosNCPDustNonCage[4]",
	"PPosNCPDustNonCage[5]",
	"PPosNCPDustNonCage[6]"
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
  
  val fOfI = populationNames.head
  val dOfI = dustNames.head
		  
  val sampleChains = faecesChains zip dustChains

  case class Sensitivity(faeces: Double, dust: Double)
  def combinedDist(fDist: Samplable[Double, Random], dDist: Samplable[Double, Random]) = new Samplable[Sensitivity, Random]{
    
    def sample(implicit r: Random) = {
      Sensitivity(fDist.sample(r), dDist.sample(r))
    }
  }
  
  val samplableChains = sampleChains.map(faeces => combinedDist(faeces._1, faeces._2))
  
  val sampleSizes = samplableChains.map (chains => smallestSampleSize(chains))
  
  val minimumSampleSizes = sampleSizes.map(a => a.last._1)
  
  // Report
  populationNames zip minimumSampleSizes foreach(println)
  
  val fullData = populationNames zip sampleSizes
  
//  fullData foreach(x => customPrint(x))
//  
//  def customPrint(x: (String, List[(Int, Double)])) = {
//    println(x._1)
//    x._2 foreach (y => println(y._1 + ", " + y._2))
//  }
  
  // Analysis code
  def smallestSampleSize(sensitivityDist: Samplable[Sensitivity, Random]) = {
    @tailrec
    def calcConf(numTrials: Int, accum: List[(Int, Double)]) : List[(Int, Double)] = {
      // Single trial Se => Multiple trials Se
      def probDetection(se: Sensitivity) = 1 - math.pow((1 - se.faeces), numTrials) * math.pow((1 - se.dust), 2)
     
      // Termination condition for drawing samples
      def terminateCondition(soFar: GenSeq[Boolean]) = {
    	val distance = metric.max(
    	    soFar.toEmpiricalTable, 
    	    soFar.take(soFar.size - chunkSize).toEmpiricalTable
    	)
//    	println(distance)
    	(distance < 0.0001) || (soFar.size > 1e8)
      }
      
      // The proportion of samples so far that are positive
      def proportionPositive(samples: GenSeq[Boolean]) = 
        samples.toEmpiricalTable.probabilities(true).value
      
      // Build model for result of 'numTrials',
      //incorporating uncertainty in test performance
      val detectionProbs = sensitivityDist map (se => Probability(probDetection(se)))
//      (1 to 100).map(x => print(detectionProbs.sample.value + ", "))
//      println()
      val model = Samplable.bernouliTrial(detectionProbs)
      
      // Build sampling distribution and finish
      //if confidence in +ve result suff high 
      val samples = new ParallelSampleBuilder(chunkSize)(model)(terminateCondition)
      
      val conf = proportionPositive(samples)
      
      if(conf > requiredConf) accum.:+((numTrials, conf))
      else calcConf(numTrials + 1, accum.:+((numTrials, conf)))  
    }
    
    calcConf(1, List())
  }
}