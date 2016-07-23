//package sampler.data
//
//import org.scalatest.path.FreeSpec
//import org.scalatest.Matchers
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
//import org.scalatest.mock.MockitoSugar
//
//import sampler.math.Random
//import sampler.olddata.DistributionBuilder;
//
//import org.mockito.Mockito._
//
//@RunWith(classOf[JUnitRunner])
//class DistributionBuilderTest extends FreeSpec with Matchers with MockitoSugar{
//
//	"Test that bernoulli samples are sane" in {
//		//Setup
//		val mockRandom = mock[Random]
//		when(mockRandom.nextBoolean(0.3)).thenReturn(true, true, false)
//		
//		// Make the thing to test
//		val instance = DistributionBuilder.bernoulli(0.3)(mockRandom)
//		
//		//Check the values coming out
//		assert(instance.sample === true)
//		assert(instance.sample === true)
//		assert(instance.sample === false)
//	}
//}