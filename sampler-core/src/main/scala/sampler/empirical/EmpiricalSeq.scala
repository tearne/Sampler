package sampler.empirical

case class EmpiricalSeq[A](seq: Seq[A]) extends Empirical[A] {
  val observationCount: Int = seq.size
  	  
  lazy val probabilityTable: Map[A, Double] = {
    val countAsDouble = seq.size.toDouble
    seq.groupBy(identity).mapValues(_.size / countAsDouble)
  }
}
