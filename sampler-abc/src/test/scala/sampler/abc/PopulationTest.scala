package sampler.abc

import org.scalatest.FreeSpec
import play.api.libs.json.JsValue

class PopulationTest extends FreeSpec {
  case class TestParams(s: String, d: Double, i: Int)
  
  "Population should" - {
    "render json" in { fail("TODO") }
    
    "parse from json" in {  
      val jsonStr = """
        {
          "generation" : 3,
          "tolerance" : 0.975,
          "acceptance-ratio" : 0.25,
          "particles" : {
      		  "a-string":  ["hay", "bee", "sea"],
            "b-double" : [ 0.3,  0.4,   0.5 ],
            "c-int" :    [ 6,    7,     8 ],
            "weight" :   [ 1,    3,     2 ]
          }
        }"""
      
      def parameterParser(map: Map[String, JsValue]): TestParams = 
        TestParams(
          map("a-string").as[String], 
          map("b-double").as[Double], 
          map("c-int").as[Int])
      
      val expected = Population[TestParams](
        Map[TestParams, Double](
            TestParams("hay", 0.3, 6) -> 1, 
            TestParams("bee", 0.4, 7) -> 3, 
            TestParams("sea", 0.5, 8) -> 2),
	      3, 
		    0.975,
	    	0.25
      )
      assertResult(expected){
        Population.fromJson(jsonStr, parameterParser _)
      }
    }
    
    "build proposal distribution using weights table" in { fail("TODO") }
  }
}