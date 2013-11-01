package sampler.abc.builder.local

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import sampler.abc.generation.IntegerModel
import sampler.abc.EncapsulatedPopulation
import sampler.abc.Particle
import sampler.run.ParallelRunner
import sampler.abc.ABCParameters
import sampler.math.Random
import org.mockito.Mockito.when
import sampler.run.Abortable
import scala.util.Success
import sampler.abc.MaxRetryException
import scala.util.Failure

class LocalPopulationBuilderTest extends AssertionsForJUnit with MockitoSugar {

  val model = IntegerModel

  val instance = new LocalPopulationBuilder {
      val jobBuilder = mock[JobBuilder]
      val particleBuilder = mock[ParticleBuilder]
      val runner = mock[ParallelRunner]
  }

  val random = Random

  val anything = 0
  
  val parameters = ABCParameters(
    anything, anything, anything,
    anything, anything, anything
  )
  
  @Test def mainTest {
    val particles = 2
    val chunks = 1
    val tolerance = 1000
    
    val abcParams = parameters.copy(
      numParticles = particles,
      particleChunking = chunks,
      startTolerance = tolerance
    )

    val p1 = new Particle(model.Parameters(1), 1, Double.MaxValue)
    val p2 = new Particle(model.Parameters(2), 1, Double.MaxValue)
    val p3 = new Particle(model.Parameters(3), 1, Double.MaxValue)
    val p4 = new Particle(model.Parameters(4), 1, Double.MaxValue)
    
    val ePop = EncapsulatedPopulation(model)(Seq(p1, p2))
    val ePop2 = EncapsulatedPopulation(model)(Seq(p3, p4))
    
    val jobs = Seq(Abortable{aborter => ePop.population})
    
    val jobResult = Seq(Success(ePop2.population))
    
    when(instance.jobBuilder.makeJobs(ePop)(abcParams, tolerance, random)).thenReturn(jobs)
    
    when(instance.runner.apply(jobs)).thenReturn(jobResult)
    
    val result = instance.run(ePop, abcParams, tolerance, random)
    
    assert(result.get.population === ePop2.population)	// Have to use population as Encapsulation happens within class
  }
  
  @Test def returnsNoneIfOnlyBadRetryException {
    val particles = 1
    val chunks = 1
    val tolerance = 1000

    val abcParams = parameters.copy(
      numParticles = particles,
      particleChunking = chunks,
      startTolerance = tolerance
    )
    
    val p1 = new Particle(model.Parameters(1), 1, Double.MaxValue)
    
    val ePop = EncapsulatedPopulation(model)(Seq(p1))
    
    val jobs = Seq(Abortable{aborter => ePop.population})
    
    val jobResult = Seq(Failure(new MaxRetryException))
    
    when(instance.jobBuilder.makeJobs(ePop)(abcParams, tolerance, random)).thenReturn(jobs)
    
    when(instance.runner.apply(jobs)).thenReturn(jobResult)
    
    val result = instance.run(ePop, abcParams, tolerance, random)
    
    assert(result === None)
  }
  
  @Test def failIfUnexpectedExceptionOccurs {
    val particles = 1
    val chunks = 1
    val tolerance = 1000
    
    val abcParams = parameters.copy(
      numParticles = particles,
      particleChunking = chunks,
      startTolerance = tolerance
    )
    
    val p1 = new Particle(model.Parameters(1), 1, Double.MaxValue)
    
    val ePop = EncapsulatedPopulation(model)(Seq(p1))
    
    val jobs = Seq(Abortable{aborter => ePop.population})
    
    val jobResult = Seq(Failure(new RuntimeException))
    
    when(instance.jobBuilder.makeJobs(ePop)(abcParams, tolerance, random)).thenReturn(jobs)
    
    when(instance.runner.apply(jobs)).thenReturn(jobResult)
    
    intercept[RuntimeException]{
      instance.run(ePop, abcParams, tolerance, random)
    }
  }
}