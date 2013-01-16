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

import java.nio.file.Paths
import sampler.io.ChainReader
import sampler.data.Empirical._
import sampler.math.Random
import sampler.data.ParallelSampleBuilder
import scala.collection.GenSeq
import sampler.data.EmpiricalMetricComponent
import sampler.data.EmpiricalTable
import scala.collection.parallel.ParSeq
import sampler.math.Statistics
import sampler.data.Empirical
import sampler.math.Probability
import sampler.data.Samplable
import scala.annotation.tailrec

object NCP_Sensitivity extends App with EmpiricalMetricComponent{
  
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

//  NCP non cage
  
  val populationNames = List(
	"PPosNCPFaecesNonCage[1]",
	"PPosNCPFaecesNonCage[2]",
	"PPosNCPFaecesNonCage[3]",
	"PPosNCPFaecesNonCage[4]",
	"PPosNCPFaecesNonCage[5]",
	"PPosNCPFaecesNonCage[6]"
  )

  // Preamble
  implicit val r = new Random()
  val stats = new Statistics
  
  // Metaparameters
  val requiredConf = 0.95
  val chunkSize = 2000

  // Run analysis
  val sampleSizes = populationNames 
		  .map (name => chains(name))
  		  .map (chain => smallestSampleSize(chain))
  
  val minimumSampleSizes = sampleSizes.map(a => a.last._1)
  
  // Report
  populationNames zip minimumSampleSizes foreach(println)
  
  val fullData = populationNames zip sampleSizes
  
  fullData foreach(x => customPrint(x))
  
  def customPrint(x: (String, List[(Int, Double)])) = {
    println(x._1)
    x._2 foreach (y => println(y._1 + ", " + y._2))
  }
  		  
  // Analysis code
  def smallestSampleSize(senstivityDist: Seq[Double]) = {
    @tailrec
    def calcConf(numTrials: Int, accum: List[(Int, Double)]) : List[(Int, Double)] = {
      // Single trial Se => Multiple trials Se
      def probDetection(p: Double) = 1 - math.pow((1 - p), numTrials)
     
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
      val detectionProbs = senstivityDist map (se => Probability(probDetection(se)))
      val model = Samplable.bernouliTrial(detectionProbs.toEmpiricalSeq)
      
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