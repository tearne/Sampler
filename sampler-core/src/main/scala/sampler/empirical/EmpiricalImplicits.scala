package sampler.empirical

trait EmpiricalImplicits{
	implicit class TableOps[A](table: Map[A, Int]) {
		def toEmpirical = EmpiricalTab(table)
	}
  
	implicit class SeqOps[A](seq: Seq[A]) {
		def toEmpirical = EmpiricalSeq(seq)
	}
}