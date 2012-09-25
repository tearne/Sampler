package sampler.math

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.math.AliasMethod._
import org.specs2.matcher.BeLessThanOrEqualTo

@RunWith(classOf[JUnitRunner])
class AliasMethodSpec extends Specification{

  "Alias method" should {
    "return some random column indexes from a list" in {
      
      val myList: java.util.List[java.lang.Double]  = java.util.Arrays.asList(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)

      println(myList)
      
      val aliasMethod = new AliasMethod(myList)
      
      println(aliasMethod.next())
      println(aliasMethod.next())
      println(aliasMethod.next())
      println(aliasMethod.next())
      println(aliasMethod.next())
      println(aliasMethod.next())
      println(aliasMethod.next())
      
      aliasMethod.next() must beBetween(0, 8)
    }
  }
}