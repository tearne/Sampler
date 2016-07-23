package sampler.empirical

import scala.language.higherKinds
import scala.collection.{GenMap, GenSeq}
import sampler.math.Partition
import sampler.statistical.StatisticalSyntax
import sampler.statistical.Statistical

case class EmpiricalTab[A](table: Map[A, Int])/* extends Empirical[A]*/ {
  val observationCount: Int = table.values.sum
  
  lazy val probabilityTable: Map[A, Double] = {
    val countAsDouble = table.values.sum.toDouble
    table.mapValues(_ / countAsDouble)
  }
}
object EmpiricalTab {
  implicit val empiricalTableIsStatistical: Statistical[EmpiricalTab] = new Statistical[EmpiricalTab]{
    def observationCount(t: EmpiricalTab[_]): Int = t.observationCount
	  def probabilityTable[A](t: EmpiricalTab[A]): Map[A, Double] = t.probabilityTable
  }
}

