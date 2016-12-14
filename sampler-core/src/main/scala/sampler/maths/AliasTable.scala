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

package sampler.maths

import scala.annotation.tailrec

/** Generates an Alias Table for sampling from a discrete probability distribution.
 *  
 *  An implementation of the Alias Method algorithm for efficient sampling from discrete probability distributions.
 *  
 *  @constructor Generates a new Alias table
 *  @param p The [[sampler.math.Partition]] containing the probabilities to be drawn from
 */

class AliasTable(probabilities: IndexedSeq[Double]) extends Serializable with RangeCheckImplicits {
  require(probabilities.areProbabilities)
  private def isCloseToOne(value: Double) = if(value > 1 - 1E-8 && value < 1 + 1E-8) true else false
  require(isCloseToOne(probabilities.sum), s"Expected probabilies to sum to 1, but got ${probabilities.sum}")
  
  val size = probabilities.size
  
  val (probability, alias) = {
  	val mean = 1.0 / size
  	
  	val initialProbability = Array.fill[Double](size)(1.0)
  	val initialAlias = Array.fill[Int](size)(0)
  	
    val small = probabilities.zipWithIndex.filter(_._1 < mean).map(_._2).toArray
    val large = probabilities.zipWithIndex.filter(_._1 >= mean).map(_._2).toArray
        
  	@tailrec
    def loop(
        small: Array[Int], 
        large: Array[Int], 
        aliasProbs: Array[Double], 
        alias: Array[Int], 
        probs: IndexedSeq[Double]
    ): (Array[Double], Array[Int]) = {
    
      if(small.isEmpty || large.isEmpty) (aliasProbs, alias)
      else {
       	val less = small.last
    		val more = large.last
    		
    		val aliasProbability = probs(less) * size
    		val rawProbability = probs(more) + probs(less) - mean
        
    		if(rawProbability >= mean)
    		  loop(
            small.dropRight(1), 
            large.dropRight(1).:+(more), 
            aliasProbs.updated(less, aliasProbability), 
            alias.updated(less, more), 
            probs.updated(more, rawProbability)
    		  )
        else
          loop(
            small.dropRight(1).:+(more), 
            large.dropRight(1), 
            aliasProbs.updated(less, aliasProbability), 
            alias.updated(less, more), 
            probs.updated(more, rawProbability)
          )
  	  }
    }
  
    loop(small, large, initialProbability, initialAlias,  probabilities)
  }
  
//  private def build(probs: IndexedSeq[Double]): (Array[Double], Array[Int]) = {
//    val average = 1.0 / size
//      
//    @tailrec
//    def loop(
//        small: Array[Int], 
//        large: Array[Int], 
//        aliasProbs: Array[Double], 
//        alias: Array[Int], 
//        probs: IndexedSeq[Double]
//  	): (Array[Double], Array[Int]) = {
//  	  if(small.isEmpty || large.isEmpty) (aliasProbs, alias)
//  	  else {
//  	   	val less = small.last
//  		  val more = large.last
//  		
//    		val aliasProbability = probs(less) * size
//    		val rawProbability = probs(more) + probs(less) - average
//        
//    		if(rawProbability >= average) loop(
//          small.dropRight(1), 
//          large.dropRight(1).:+(more), 
//          aliasProbs.updated(less, aliasProbability), 
//          alias.updated(less, more), 
//          probs.updated(more, rawProbability)
//  		  )
//        else loop(
//          small.dropRight(1).:+(more), 
//          large.dropRight(1), 
//          aliasProbs.updated(less, aliasProbability), 
//          alias.updated(less, more), 
//          probs.updated(more, rawProbability)
//        )
//      }
//    }
//  
//    val initialProbability = Array.fill[Double](size)(1.0)
//  	val initialAlias = Array.fill[Int](size)(0)
//  	
//    val small = probabilities.zipWithIndex.filter(_._1 < average).map(_._2).toArray
//    val large = probabilities.zipWithIndex.filter(_._1 >= average).map(_._2).toArray
//    
//    loop(small, large, initialProbability, initialAlias,  probabilities)
//  }

  /** Return a random value drawn from the distribution */
  def next(rand: Random): Int = {
    val column = rand.nextInt(probability.size)
    val coinToss = rand.nextDouble() < probability(column)
  
    if(coinToss) column 
    else alias(column)
  }
}
