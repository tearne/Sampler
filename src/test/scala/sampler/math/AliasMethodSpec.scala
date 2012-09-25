package sampler.math

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.math.AliasMethod._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.math.AliasMethod
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AliasMethodSpec extends Specification{

  "Alias method" should {
    "return some random column indexes from a list" in {
      
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
  }
}