package sampler.math

class Alias(probabilities: Array[Double], rand: Random) {

  // Take indexed seq of probabilites as like AliasWrapper
  
  // prevent repeated calls to probabilities.size
  
	var probability = Array.fill[Double](probabilities.size)(0)
	var alias = Array.fill[Int](probabilities.size)(0)
	
	val average = 1.0 / probabilities.size
	
	var small: List[Int] = List()
	var large: List[Int] = List()
	
	for(i <- 0 until probabilities.size) {
		if(probabilities(i) > average)
			large = large.:+(i)
		else
			small = small.:+(i)
	}
	
	while(!small.isEmpty && !large.isEmpty) {
	  val less = small.last
	  small = small.dropRight(1)
	  
	  val more = large.last
	  large = large.dropRight(1)
	  
	  val newProb1 = probabilities(less) * probabilities.size
	  probability(less) = newProb1
	  
	  alias(less) = more
	  
	  val newProb2 = probabilities(more) + probabilities(less) - average
	  
	  probabilities(more) = newProb2
	  
	  if(newProb2 >= average)
	    large = large.:+(more)
	  else
	    small = small.:+(more)
	}
	
	// Could do in initialisation
	
	while(!small.isEmpty) { 
	  probability(small.last) = 1.0
	  small = small.dropRight(1)
	}
	while(!large.isEmpty) { 
		probability(large.last) = 1.0
		large = large.dropRight(1)
	}
	
	System.out.println("Probability: " + probability(0) + 
        		", " + probability(1) + ", " + probability(2) + 
        		", " + probability(3));
        
    System.out.println("Alias: " + alias(0) + 
        		", " + alias(1) + ", " + alias(2) + 
        		", " + alias(3));
        
    def next: Int = {
      val column = rand.nextInt(probability.size)
      
      val coinToss = rand.nextDouble() < probability(column)
      
      if(coinToss) column else alias(column)
    }
}