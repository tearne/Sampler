package sampler.abc.builder.local

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import sampler.io.Logging
import sampler.abc.builder.ParticleBuilderComponent
import sampler.abc.generation.IntegerModel
import sampler.abc.Particle
import sampler.run.Abortable
import sampler.math.Random
import sampler.abc.ABCParameters
import sampler.abc.EncapsulatedPopulation
import org.mockito.Mockito.when
import org.mockito.Matchers._
import sampler.run.Aborter

class JobRBuilderComponentTest extends AssertionsForJUnit with MockitoSugar {

  @Test def mainTest {
    val instance = new JobBuilderComponent with Logging with ParticleBuilderComponent {
      val jobBuilder = new JobBuilder{}
      val particleBuilder = mock[ParticleBuilder]
    }
    
    val model = IntegerModel
    
    val r = Random

    val anything = 0
  
    val tolerance = 1e-6
    val particles = 3
    val chunkSizes = 1
    
    val parameters = ABCParameters(
      anything, particles, tolerance,
      anything, anything, chunkSizes
    )
    
    val p1 = new Particle(mock[model.Parameters], 1, Double.MaxValue)
    val p2 = new Particle(mock[model.Parameters], 1, Double.MaxValue)
    val p3 = new Particle(mock[model.Parameters], 1, Double.MaxValue)
    
    val ePop = EncapsulatedPopulation(IntegerModel)(Seq(p1,p2,p3))
    
    val aborter = mock[Aborter]
    
    when(instance.particleBuilder.apply(model)(ePop.population, chunkSizes, tolerance, aborter, parameters, r))
    	.thenReturn(Seq(p1), Seq(p2), Seq(p3))
    
    val jobList = instance.jobBuilder.makeJobs(ePop)(parameters, tolerance, r)
    
    val results = jobList.map{job => job.run(aborter)}
    
    assert(results === Seq(Seq(p1), Seq(p2), Seq(p3)))
  }
}