package sampler.empirical

import sampler.statistical.Statistical

case class EmpiricalSeq[A](seq: Seq[A])/* extends Empirical[A]*/ {
  val observationCount: Int = seq.size
  	  
  lazy val probabilityTable: Map[A, Double] = {
    val countAsDouble = seq.size.toDouble
    seq.groupBy(identity).mapValues(_.size / countAsDouble)
  }
}

object EmpiricalSeq {
  implicit val empiricalSeqIsStatistical: Statistical[EmpiricalSeq] = new Statistical[EmpiricalSeq]{
    def observationCount(t: EmpiricalSeq[_]): Int = t.observationCount
	  def probabilityTable[A](t: EmpiricalSeq[A]): Map[A, Double] = t.probabilityTable
  }
}