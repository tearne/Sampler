package sampler.spike.distribution.tearne

import scala.collection.{GenSeq, GenMap}

trait EmpiricalImplicits {
  implicit class SeqOps[A](genSeq: GenSeq[A]) {
		def empirical = SeqDist[A](genSeq.toIndexedSeq)
	}
	
	implicit class MapOps[A](table: GenMap[A,Double]) {
		def empirical = TableDist[A](table.seq.toMap)
	}
}