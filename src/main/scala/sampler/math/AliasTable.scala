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

package sampler.math

import scala.annotation.tailrec

class AliasTable(origProbs: IndexedSeq[Probability]) extends Serializable{
    val probabilities = origProbs.map(v => v.value)
  
    assert(isEqualOne(probabilities.sum), "Cannot use the alias method if probabilities don't sum to one")
  
    private def isEqualOne(value: Double) = if(value > 1 - 1E-8 && value < 1 + 1E-8) true else false
    
    val (probability, alias) = construct(probabilities)
	
    def construct(probs: IndexedSeq[Double]): (Array[Double], Array[Int]) = {
	  val arraySize = probabilities.size
	
	  val average = 1.0 / arraySize
      
	  @tailrec
	    def loop(
    	  small: Array[Int], 
    	  large: Array[Int], 
    	  aliasProbs: Array[Double], 
    	  alias: Array[Int], 
    	  probs: IndexedSeq[Double]
    	): (Array[Double], Array[Int]) = {
	    
		  if(small.isEmpty || large.isEmpty) {
		   	(aliasProbs, alias)
		  } else {
		   	val less = small.last
			val more = large.last
      
			val aliasProbability = probs(less) * arraySize
			val rawProbability = probs(more) + probs(less) - average
      
			if(rawProbability >= average)
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
	  
      val initialProbability = Array.fill[Double](arraySize)(1.0)
	  val initialAlias = Array.fill[Int](arraySize)(0)
	  	
      val small = probabilities.zipWithIndex.filter(_._1 <= average).map(_._2).toArray
      val large = probabilities.zipWithIndex.filter(_._1 > average).map(_._2).toArray
    		
      loop(small, large, initialProbability, initialAlias, probabilities)
    }
    
    def next(rand: Random): Int = {
      val column = rand.nextInt(probability.size)
      
      val coinToss = rand.nextDouble() < probability(column)
      
      if(coinToss) column else alias(column)
    }
}
