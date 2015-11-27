package sampler.abc.core

import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.FreeSpec
import sampler.data.DistributionBuilder
import sampler.abc.config.ABCConfig
import sampler.abc.config.JobParameters
import org.mockito.Matchers

class ReporterTest extends FreeSpec with MockitoSugar{
	type T = Int
	val thousandParticles = 1000
	val iteration500 = 500
	val tolerance = 0.001
	
	"Generates a report of a completed generation" in {
		//TODO output the weights table, not a sample from it?
		val distBuilder = mock[DistributionBuilder]
    val config = ABCConfig(
    		JobParameters(thousandParticles,0,0), 
    		null, 
    		null
    )
		
		val instance = new Reporter(distBuilder, null, config)
		
		val particleWeights = mock[Map[T, Double]]
		when(distBuilder.fromWeightsTable(Matchers.eq(particleWeights))(any()))
			.thenReturn(DistributionBuilder.continually(7))
    
		val generation = Generation[Int](
        //null,
        iteration500,
        particleWeights,
        tolerance
    )
      
    val report = instance.build(generation)
    
    val posterior = report.posterior
    
    assert(report.generationId === iteration500)
    assert(report.tolerance === tolerance)
    assert(posterior.length === thousandParticles)
    assert(!posterior.exists(_ != 7))
  }
}