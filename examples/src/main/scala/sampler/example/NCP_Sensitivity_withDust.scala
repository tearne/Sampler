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

package sampler.example

import sampler.data.Empirical
import java.nio.file.Paths
import sampler.io.ChainReader
import sampler.math.Random
import sampler.math.StatisticsComponent
import scala.annotation.tailrec
import scala.collection.GenSeq
import sampler.math.Probability
import sampler.data.Samplable
import sampler.data.Empirical._
import sampler.data.ParallelSampleBuilder
import sampler.data.Empirical

/** A example showing how the various data structures and mathematical functions in the toolkit 
 *  can be applied to epidemiologically relevant questions relating to sample sizes and experiment design
 *  
 *  This example is adapted from a real-life question trying to determine the optimum number of samples
 *  required to have a defined confidence of detecting infection if it is present
 *  
 *  The sampling protocol used in this example had a requirement of 2 dust samples being taken. The 
 *  probability of detection using 2 dust samples and an increasing number of faecal samples was calculated
 *  until the required confidence had been exceeded. This analysis is performed for both caged and non-caged
 *  birds
 */

object NCP_Sensitivity_withDust extends App with StatisticsComponent {

  val pathspec = Paths.get("", "src", "main", "scala", "sampler", "example", "NCP_data", "coda")
  
  /** The sensitivity of each sample type is taken from WinBUGS estimates */
  // Note data has been randomly generated for this example
  val chains = ChainReader(pathspec.toString())

  val experiementNames = List(
      "Caged",
      "NonCaged"
  )
  
  val faecesNames = List(
	"FaecesCage",
	"FaecesNonCage"
  )
  
  val dustNames = List(
	"DustCage",
	"DustNonCage"
  )

  implicit val r = Random
  
  val requiredConf = 0.95
  val chunkSize = 2000

  // Convert chains into Samplable objects that can be sampled at random 
  val faecesChains = faecesNames 
		  .map (name => chains(name))
		  .map (list => list.toEmpiricalSeq.toSamplable)
		  
  val dustChains = dustNames 
		  .map (name => chains(name))
		  .map (list => list.toEmpiricalSeq.toSamplable)
  
  val sampleChains = faecesChains zip dustChains

  case class Sensitivity(faeces: Double, dust: Double)
  
  /** Returns a new Samplable which gives an individual sensitivity for both sample types */
  def combinedDist(fDist: Samplable[Double], dDist: Samplable[Double]) = new Samplable[Sensitivity]{
    
    def sample = {
      Sensitivity(fDist.sample(), dDist.sample())
    }
  }
  
  // Combine data from the two test types into one Samplable
  val samplableChains = sampleChains.map(tests => combinedDist(tests._1, tests._2))
  
  // Works out the minimum sample size for each set of birds in the chains
  val sampleSizes = samplableChains.map (chains => smallestSampleSize(chains))
  
  // Selects and prints the minimum sample size
  val minimumSampleSizes = sampleSizes.map(a => a.last._1)
  experiementNames zip minimumSampleSizes foreach(x => summaryPrint(x._1, x._2))
  
  def summaryPrint(s: String, n: Int) = {
    println(s"For ${s} birds the miniumum number of faecal samples to acheive ${requiredConf} confidence is ${n}\n")
  }
  
//  Code to print out the sensitivity for each incremental sample size
//  val fullData = faecesNames zip sampleSizes
//  fullData foreach(x => customPrint(x))
//  
//  def customPrint(x: (String, List[(Int, Double)])) = {
//    println(x._1)
//    x._2 foreach (y => println(y._1 + ", " + y._2))
//  }
  
  // Analysis code
  /** A recursive function to work out the probability of seeing a positive result, with increasing
   *  sample size until the required confidence is reached
   */
  def smallestSampleSize(sensitivityDist: Samplable[Sensitivity]) = {
    
    @tailrec
    def calcConf(numTrials: Int, accum: List[(Int, Double)]) : List[(Int, Double)] = {
      /** Returns the probability of detection given x number of feacal samples and 2 dust samples 
       * 
       *  Single trial Se => Multiple trials Se
       **/
      def probDetection(se: Sensitivity) = 1 - math.pow((1 - se.faeces), numTrials) * math.pow((1 - se.dust), 2)
     
      /** Returns true if the sampling should terminate
       *  
       *  Termination should occur either when convergence has been achieved
       *  or when enough samples have been taken
       */
      def terminateCondition(soFar: GenSeq[Boolean]) = {
    	val distance = maxDistance(
    	    soFar.toEmpiricalTable, 
    	    soFar.take(soFar.size - (chunkSize-1)).toEmpiricalTable			// TODO confirm -1 change
    	)
    	
    	(distance < 0.0001) || (soFar.size > 1e8)
      }
      
      /** Returns the number of positive samples as a proportion of the total **/
      def proportionPositive(samples: GenSeq[Boolean]) = 
        samples.toEmpiricalTable.probabilityTable(true).value
      
      // Coneverts Samplable[Double] to Samplable[Proability], maintaining uncertainty in calculations
      val detectionProbs = sensitivityDist map (se => Probability(probDetection(se)))

      // Returns a Samplable[Boolean] which represents the likelihood of the sampling 
      // strategy successfully detecting disease
      def makeModel(s: Samplable[Probability]) = new Samplable[Boolean]{
        
        def sample() = r.nextBoolean(s.sample)
      }
      
      val model = makeModel(detectionProbs)
      
      // Build sampling distribution and terminate when converged
      val samples = new ParallelSampleBuilder(chunkSize)(model)(terminateCondition)
      
      // Determine the proportion of positive samples (confidence)
      // iterate with high sample size if proportion not high enough
      val conf = proportionPositive(samples)
      if(conf > requiredConf) accum.:+((numTrials, conf))
      else calcConf(numTrials + 1, accum.:+((numTrials, conf)))  
    }
    
    calcConf(1, List())
  }
}