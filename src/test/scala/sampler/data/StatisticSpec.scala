package sampler.data

import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StatisticSpec extends Specification with Mockito{
	"Statistic" should {
		"calculate the mean" in {
			val empirical = mock[Empirical[Double]]
			val counts = List((1.1,10), (2.2,20), (3.3, 30)).toMap
			empirical.counts returns counts
			empirical.size returns 60
			
			Statistic.mean(empirical) mustEqual (11+44+99)/60.0
		}
	}
}