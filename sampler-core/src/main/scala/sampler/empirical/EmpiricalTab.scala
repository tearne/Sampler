package sampler.empirical

case class EmpiricalTab[A](table: Map[A, Int]) extends Empirical[A] {
  val observationCount: Int = table.values.sum
  
  lazy val probabilityTable: Map[A, Double] = {
    val countAsDouble = table.values.sum.toDouble
    table.mapValues(_ / countAsDouble)
  }
}
