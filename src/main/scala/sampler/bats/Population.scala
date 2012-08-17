package sampler.bats

import scala.util.Random

class Population(popSize : Int, percentInfected: Double) {

	// TODO add checker for valid percentInfected
	
	var pop: List[Boolean] = List()
	
	val numInfected = (popSize * percentInfected).round.toInt
	val numNotInfected = popSize - numInfected
	
	val positive = List(true)
	val negative = List(false)
	
	for(x <- 0 until numInfected)
		pop = pop ++ positive
		
	for(x <- 0 until numNotInfected)
		pop = pop ++ negative
		
	// TODO add check to make sure calculations have been performed correctly and population is right size
		
	def sampleWithReplacement(numToSample: Int) : Int = {
		val rand = new Random()
		
		var numDetected = 0
		
		for(i <- 0 until numToSample) {
			val indexToSample = rand.nextInt(popSize)
			
			if(pop(indexToSample)) 
				numDetected = numDetected.+(1)
		}
		
		numDetected
	}
}