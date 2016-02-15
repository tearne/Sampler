package sampler.spike.distribution.tearne

import scala.collection.{GenSeq, GenMap}
import sampler.math.Random

trait DistributionImplicits {
  implicit class SeqOps[A](genSeq: GenSeq[A]) {
		def distribution = Distribution.fromSequence(genSeq.toIndexedSeq)
	}
	
	implicit class MapOps[A](table: GenMap[A,Double]) {
		def distribution = Distribution.fromTable(table.seq.toMap)
	}
	
	implicit class FunctionOps[A](f: Random => A) {
	  def distribution = Distribution.build(f)
	}
}