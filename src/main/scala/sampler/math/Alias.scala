package sampler.math

class Alias(origProbs: IndexedSeq[Double], rand: Random) {

    var probabilities = origProbs
  
    val arraySize = probabilities.size
    
	var probability = Array.fill[Double](arraySize)(1.0)
	var alias = Array.fill[Int](arraySize)(0)
	
	val average = 1.0 / arraySize
	
	var small: List[Int] = List()
	var large: List[Int] = List()
	
	for(i <- 0 until arraySize) {
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
	  
	  val newProb1 = probabilities(less) * arraySize
	  probability(less) = newProb1
	  
	  alias(less) = more
	  
	  val newProb2 = probabilities(more) + probabilities(less) - average
	  
	  probabilities = probabilities.updated(more, newProb2)
	  
	  if(newProb2 >= average)
	    large = large.:+(more)
	  else
	    small = small.:+(more)
	}
	
/*	System.out.println("Probability: " + probability(0) + 
        		", " + probability(1) + ", " + probability(2) + 
        		", " + probability(3));
        
    System.out.println("Alias: " + alias(0) + 
        		", " + alias(1) + ", " + alias(2) + 
        		", " + alias(3));*/
        
    def next: Int = {
      val column = rand.nextInt(probability.size)
      
      val coinToss = rand.nextDouble() < probability(column)
      
      if(coinToss) column else alias(column)
    }
}