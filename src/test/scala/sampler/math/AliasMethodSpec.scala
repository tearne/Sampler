package sampler.math

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.math.AliasMethod._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mock.Mockito

@RunWith(classOf[JUnitRunner])
class AliasMethodSpec extends Specification with Mockito{

  "Alias method" should {
    "return some random column indexes based on a probability list" in {
      
      val myList: java.util.List[java.lang.Double]  = java.util.Arrays.asList(0.1, 0.1, 0.4, 0.4)

      val aliasMethod = new AliasMethod(myList)
      
      var resultsList: List[Int] = List()
      
      for(i <- 0 until 1000000) {
    	  resultsList = resultsList.+:(aliasMethod.next())
      }

      (resultsList.count(_ == 0) must beBetween(99000, 101000)) and
      (resultsList.count(_ == 1) must beBetween(99000, 101000)) and
      (resultsList.count(_ == 2) must beBetween(399000, 401000)) and
      (resultsList.count(_ == 3) must beBetween(399000, 401000)) 
    }
    
    "return indexes based on mock returns" in {
    	val r = mock[Random]
    	val myList: java.util.List[java.lang.Double]  = java.util.Arrays.asList(0.1, 0.1, 0.4, 0.4)
    			
    	r.nextInt(4) returns (0, 1, 2, 3)
    	r.nextDouble() returns 0.01
    			
    	val aliasMethod = new AliasMethod(myList, r) 
    	
    	(aliasMethod.next() mustEqual 0) and
    	(aliasMethod.next() mustEqual 1) and
    	(aliasMethod.next() mustEqual 2) and
    	(aliasMethod.next() mustEqual 3)
    }
    
    "FAILING TEST: ignore contents --> rewrite Java Class in Scala" in {
      val r = mock[Random]
      val myList: java.util.List[java.lang.Double]  = java.util.Arrays.asList(0.1, 0.1, 0.4, 0.4)
      
      r.nextInt(4) returns 1
      r.nextDouble() returns 0.01
      
      val aliasMethod = new AliasMethod(myList, r) 
      
      aliasMethod.next() mustEqual 2
    }
  }
}