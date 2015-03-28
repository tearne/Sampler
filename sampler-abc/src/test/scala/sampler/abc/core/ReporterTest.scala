package sampler.abc.core

import org.scalatest.FreeSpec
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers._

class ReporterTest extends FreeSpec with MockitoSugar{
	type T = Int
	val thousandParticles = 1000
	val iteration500 = 500
	val tolerance = 0.001
	
	"Generates a report of a completed generation" in {
		val distBuilder = mock[DistributionBuilder]
    val instance = new Reporter(distBuilder, null)
		
		val particleWeights = mock[Map[T, Double]]
		when(distBuilder.fromWeightsTable(particleWeights)(any()))
			.thenReturn(DistributionBuilder.continually(7))
    
		val generation = Generation[Int](
        null,
        iteration500,
        Map(1 -> 0.5, 2 -> 0.5),
        tolerance
    )
      
    val config = ABCConfig(JobParameters(thousandParticles,0,0), null, null)
    
    val report = instance.buildReport(generation, config)
    
    val posterior = report.posterior
    
    assert(report.generationId === iteration500)
    assert(report.tolerance === tolerance)
    assert(posterior.length === thousandParticles)
    assert(!posterior.exists(_ != 7))
  }
}