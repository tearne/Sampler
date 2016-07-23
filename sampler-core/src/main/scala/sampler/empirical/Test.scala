package sampler.empirical

import sampler.statistical.StatisticalSyntax

object Test extends App with StatisticalSyntax with EmpiricalImplicits{
  val empirical = Seq("A", "A", "B", "B", "B", "C").toEmpirical
  val statistic = empirical.rightTail("B")
  val pTab = empirical.probabilityTable
  
//  Seq("A", "A", "B", "B", "B", "C").probabilityTable
//  Map("A" -> 3, "B" -> 5, "C" -> 1).probabilityTable
  
  val empiricalTable = Map("A" -> 3, "B" -> 5, "C" -> 1).toEmpirical
  val stat = empiricalTable.rightTail("B")
  val t = empiricalTable.probabilityTable
}