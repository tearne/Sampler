package sampler.abc

import org.scalatest.FreeSpec
import play.api.libs.json.JsValue
import org.scalatest.Matchers

class PopulationTest extends FreeSpec with Matchers {
  case class TestParams(s: String, d: Double, i: Int)
  
  "Population should" - {
    "render json" in { fail("TODO") }
    
    "Calculate a consolidated weights table" in {
      val weightedParticles = Seq(
          Weighted(Scored(1, Seq(0.1)), 0.1),
          Weighted(Scored(1, Seq(0.2)), 0.2),
          Weighted(Scored(2, Seq(0.3)), 0.3),
          Weighted(Scored(2, Seq(0.4)), 0.4)
      )
      
      val instance = Population(weightedParticles, 0,0,0)
      val result = instance.consolidatedWeightsTable
      
      val tolerance = 1e-6
      
      result.getOrElse(1, 0.0) should be(0.3 +- tolerance)
      result.getOrElse(2, 0.0) should be(0.7 +- tolerance)
    }
    
    "parse from json" in {  
      val jsonStr = """
        {
          "generation" : 3,
          "tolerance" : 0.975,
          "acceptance-ratio" : 0.25,
          "particle-details" : [
            {
              "p": {"a-string": "hay", "b-double": 0.3, "c-int": 6},
              "s": [1, 11, 111],
              "w": 1.1
            },{
              "p": {"a-string": "bee", "b-double": 0.4, "c-int": 7},
              "s": [2, 22, 222],
              "w": 2.2
            },{
              "p": {"a-string": "sea", "b-double": 0.5, "c-int": 8},
              "s": [3, 33, 333],
              "w": 3.3
            }
          ]
        }"""
      
      def parameterParser(map: Map[String, JsValue]): TestParams = 
        TestParams(
          map("a-string").as[String], 
          map("b-double").as[Double], 
          map("c-int").as[Int]
        )
      
      val id = 0
      val expected = Population[TestParams](
        Seq(
            Weighted(Scored(TestParams("hay", 0.3, 6), Seq(1,11,111), id), 1.1), 
            Weighted(Scored(TestParams("bee", 0.4, 7), Seq(2,22,222), id), 2.2), 
            Weighted(Scored(TestParams("sea", 0.5, 8), Seq(3,33,333), id), 3.3)
        ),
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