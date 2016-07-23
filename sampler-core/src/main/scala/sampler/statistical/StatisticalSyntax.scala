package sampler.statistical

import scala.language.higherKinds

trait StatisticalSyntax{
  implicit class StatisticalOps[A, T[_]](thiz: T[A]){
    def probabilityTable(implicit instance: Statistical[T]): Map[A, Double] = 
      instance.probabilityTable(thiz)
    
    def observatationCount(implicit instance: Statistical[T]): Int = 
      instance.observationCount(thiz)
      
    /** The proportion (0-1 range) of items in the Empirical which are greater than or equal to supplied value (inclusive)
     *  
     *  @param itemInclusive The value of interest, return value is inclusive of this value
     *  @return Probability representing the proportion of items in the right tail
     **/
  	def rightTail(itemInclusive: A)(implicit o: Ordering[A], instance: Statistical[T]): Double = {
  		val pTable = probabilityTable
      val result = pTable.keys
  		  .toList
  		  .sorted
  		  .dropWhile( i => o.lt(i,itemInclusive) )
  		  .foldLeft(0.0){ case (acc, i) => acc + pTable(i) }
  		result
  	}
  }
}