package sampler.distribution

import scala.collection.{GenSeq, GenMap}
import sampler.maths.Random

// Type enrichments to easily make distributions from collections
trait DistributionImplicits {
  implicit class SeqOps[A](genSeq: GenSeq[A]) {
		def toDistribution = Distribution.fromSequence(genSeq.toIndexedSeq)
	}
	
	implicit class MapOps[A](weightsTable: GenMap[A,Double]) {
		def toDistribution = Distribution.fromWeightsTable(weightsTable.seq.toMap)
	}
	
	implicit class FunctionOps[A](f: Random => A) {
	  def toDistribution = Distribution.from(f)
	}
}