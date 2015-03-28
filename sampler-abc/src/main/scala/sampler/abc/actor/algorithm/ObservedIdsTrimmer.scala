package sampler.abc.actor.algorithm

import scala.collection.immutable.Queue

class ObservedIdsTrimmer(memoryGenerations: Int, numParticles: Int) {
	assert(memoryGenerations < numParticles)
	def apply(queuedIds: Queue[Long]) = {
		val maxNum = memoryGenerations * numParticles

		val queueSize = queuedIds.size
		
		if(queueSize >= maxNum) {
		  val reducedNum = (memoryGenerations -1) * numParticles
		  val toDrop = queueSize - reducedNum
		  queuedIds.drop(toDrop)
		} else queuedIds
	}
}