package sampler.math

class Alias(probabilities: IndexedSeq[Double], rand: Random) {

    lazy val arraySize = probabilities.size
    
	lazy val initialProbability = Array.fill[Double](arraySize)(1.0)
	lazy val initialAlias = Array.fill[Int](arraySize)(0)
	
	lazy val average = 1.0 / arraySize
	
    lazy val small = probabilities.zipWithIndex filter (_._1 <= average) map (_._2) toArray
    lazy val large = probabilities.zipWithIndex filter (_._1 > average) map (_._2) toArray
    
    lazy val (probability, alias) = construct(small, large, initialProbability, initialAlias, probabilities)
	
    def construct(small: Array[Int], large: Array[Int], aliasProbs: Array[Double], alias: Array[Int], probs: IndexedSeq[Double]): 
    		(Array[Double], Array[Int]) = {
      if(small.isEmpty || large.isEmpty) {
        (aliasProbs, alias)
      } else {
    	val less = small.last
        val more = large.last
      
        val aliasProbability = probs(less) * arraySize
        val rawProbability = probs(more) + probs(less) - average
      
        if(rawProbability >= average)
	      construct(
	          small.dropRight(1), 
	          large.dropRight(1).:+(more), 
	          aliasProbs.updated(less, aliasProbability), 
	          alias.updated(less, more), 
	          probs.updated(more, rawProbability)
	      )
	    else
		  construct(
		      small.dropRight(1).:+(more), 
		      large.dropRight(1), 
		      aliasProbs.updated(less, aliasProbability), 
		      alias.updated(less, more), 
		      probs.updated(more, rawProbability)
		  )
      }
    }
    
//	System.out.println("Probability: " + probability(0) + 
//        		", " + probability(1) + ", " + probability(2) + 
//        		", " + probability(3));
//        
//    System.out.println("Alias: " + alias(0) + 
//        		", " + alias(1) + ", " + alias(2) + 
//        		", " + alias(3));
        
    def next: Int = {
      val column = rand.nextInt(probability.size)
      
      val coinToss = rand.nextDouble() < probability(column)
      
      if(coinToss) column else alias(column)
    }
}