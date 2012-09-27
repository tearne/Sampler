package sampler.math

import java.util.ArrayList

class AliasWrapper(probabilities: List[Double]) {
		  
  if(!isEqualOne(probabilities.sum)) throw new ProbabilityException("Cannot use the alias method if probabilities don't sum to one")
  
  import collection.JavaConversions.asJavaCollection
  import collection.JavaConversions._
  
  val javaProbs = (new java.util.ArrayList(asJavaCollection(probabilities))).asInstanceOf[java.util.List[java.lang.Double]]
  
  val aliasMethod = new AliasMethod(javaProbs)
  
  def sample() = {
    aliasMethod.next()
  }
  
  private def isEqualOne(value: Double) = if(value > 1 - 1E-8 && value < 1 + 1E-8) true else false
}