package sampler.abc.builder.local

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import sampler.abc.generation.IntegerModel
import sampler.abc.EncapsulatedPopulation
import sampler.run.ParallelRunner
import sampler.abc.parameters
import sampler.math.Random
import org.mockito.Mockito.when
import sampler.run.Abortable
import scala.util.Success
import sampler.abc.MaxRetryException
import scala.util.Failure
import sampler.abc.Weighted
import sampler.abc.parameters.JobParameters
import sampler.abc.parameters.AlgorithmParameters
import sampler.abc.parameters.ABCParameters
import scalaz._
import Scalaz._

class LocalPopulationBuilderTest extends AssertionsForJUnit with MockitoSugar {

  val model = IntegerModel

  val instance = new LocalPopulationBuilder {
      val jobBuilder = mock[JobBuilder]
      val particleBuilder = mock[ParticleBuilder]
      val runner = mock[ParallelRunner]
  }

  val random = Random

  val anything = 0
  val twoParticles = 2
  val chunkSize = 10
  
  val abcParams = ABCParameters(
    JobParameters(twoParticles, anything, 1), 
    AlgorithmParameters(anything, chunkSize)
  )
  
  val tolerance = 1000
  
  @Test def returnsANewPopulation {
    import model._

    val p1 = mock[Weighted[ParameterSet]]
    val p2 = mock[Weighted[ParameterSet]]
    val p3 = mock[Weighted[ParameterSet]]
    val p4 = mock[Weighted[ParameterSet]]
    
    val previousEPop = EncapsulatedPopulation(model)(Seq(p1, p2))
    val newEPop = EncapsulatedPopulation(model)(Seq(p3, p4))
    
    val mockJobs = Seq(Abortable{aborter => previousEPop.population})
    val mockResult = Seq(Success(newEPop.population))
    
    when(instance.jobBuilder.makeJobs(previousEPop)(abcParams, tolerance, random)).thenReturn(mockJobs)
    when(instance.runner.apply(mockJobs)).thenReturn(mockResult)
    
    val result = instance.run(previousEPop, abcParams, tolerance, random)
    
    assert(result.get.population == newEPop.population)	// Have to use population as Encapsulation happens within class
  }
  
  @Test def returnsNoneIfOnlyBadRetryException {
    val p1 = mock[Weighted[model.ParameterSet]]//
    
    val ePop = EncapsulatedPopulation(model)(Seq(p1))
    
    val jobs = Seq(Abortable{aborter => ePop.population})
    
    val jobResult = Seq(Failure(new MaxRetryException))
    
    when(instance.jobBuilder.makeJobs(ePop)(abcParams, tolerance, random)).thenReturn(jobs)
    when(instance.runner.apply(jobs)).thenReturn(jobResult)
    
    val result = instance.run(ePop, abcParams, tolerance, random)
    
    assert(result == None)
  }
  
  @Test def failIfUnexpectedExceptionOccurs {
    val p1 = mock[Weighted[model.ParameterSet]]
    
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