package sampler.bats

import scala.util.Random

class Population(val popSize : Int, val percentInfected: Double) {

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
		
	val rand = new Random()

	def sampleWithReplacement(numToSample: Int) : Int = {
		
		var numDetected = 0
		
		for(i <- 0 until numToSample) {
			val indexToSample = rand.nextInt(popSize)
			
			if(pop(indexToSample)) 
				numDetected = numDetected.+(1)
		}
		
		numDetected
	}
	
	def sampleWithoutReplacement(numToSample: Int) : Int = {
		
		def removeElement(index: Int, population : List[Boolean]) : List[Boolean] = {
			val (start, _ :: end) = population.splitAt(index)
	
			start ::: end
		}
		
		var popToSample = pop
		
		var numDetected = 0
		
		for(i <- 0 until numToSample){
			val indexToSample = rand.nextInt(popToSample.size)
			
			if(popToSample(indexToSample)) 
				numDetected = numDetected.+(1)
					
			popToSample = removeElement(indexToSample, popToSample)
		}
		
		numDetected
	}
}