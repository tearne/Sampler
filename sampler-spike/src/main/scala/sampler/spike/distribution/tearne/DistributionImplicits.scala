package sampler.spike.distribution.tearne

import scala.collection.{GenSeq, GenMap}
import sampler.math.Random

trait DistributionImplicits {
  implicit class SeqOps[A](genSeq: GenSeq[A]) {
		def distribution = EmpiricalSeq[A](genSeq.toIndexedSeq)
	}
	
	implicit class MapOps[A](table: GenMap[A,Double]) {
		def distribution = EmpiricalTable[A](table.seq.toMap)
	}
	
	implicit class FunctionOps[A](f: Random => A) {
	  def distribution = Explicit[A](f)
	}
}