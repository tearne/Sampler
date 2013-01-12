package sampler.math

import java.util.ArrayList

//
//TODO Delete this class now that we have the AliasTable?
//
class AliasWrapper(probabilities: IndexedSeq[Probability]) {
	val probsAsDouble = probabilities.map(_.value)

  if(!isEqualOne(probsAsDouble.sum)) throw new ProbabilityException("Cannot use the alias method if probabilities don't sum to one")
  
  import collection.JavaConversions.asJavaCollection
  import collection.JavaConversions._
  
  val javaProbs = (new java.util.ArrayList(asJavaCollection(probsAsDouble))).asInstanceOf[java.util.List[java.lang.Double]]
  
  val aliasMethod = new AliasMethod(javaProbs)
  
  def sample() = {
    aliasMethod.next()
  }
  
  private def isEqualOne(value: Double) = if(value > 1 - 1E-8 && value < 1 + 1E-8) true else false
}