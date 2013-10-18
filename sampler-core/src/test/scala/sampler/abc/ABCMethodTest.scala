package sampler.abc

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.when
import org.mockito.Matchers.anyObject
import sampler.math.Random
import sampler.abc.population.PopulationBuilder
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.Success
import sampler.data.Distribution
import sampler.math.StatisticsComponent
import sampler.math.StatisticsComponent

class ABCMethodTest extends AssertionsForJUnit {

  @Test
  def testInitialisation {
    val parameters = mock[ABCParameters]
    when(parameters.numParticles).thenReturn(5)

    implicit val r = Random
    
    class TestModel extends ABCModel with StatisticsComponent {
      
      case class Parameters(i: Int) extends ParametersBase with Serializable {
        def perturb() = Parameters(i)
        def perturbDensity(that: Parameters) = 1.0
      }
      
      val observations = Observations(2)
      
      case class Observations(n: Int) extends ObservationsBase with Serializable{}

      case class Output(simulated: Observations) extends OutputBase with Serializable{
        def distanceTo(obs: Observations): Double = 0.0
      }
          
      def modelDistribution(p: Parameters, obs: Observations) = new Distribution[Output] with Serializable {
        val s = Distribution.uniform(1, 1)
    	override def sample = Output(Observations(s.sample))
      }
      
      val prior = new Prior[Parameters] with Serializable{
        val dist = Distribution.continually(1)
        
        def density(p: Parameters) = 1.0
        def sample() = Parameters(dist.sample)
      }
    }
   
    val myModel = new TestModel
    
    val abcMethod = new ABCMethod(myModel, parameters, Random)
    
    val expectedParticle = Particle(myModel.Parameters(1), 1.0, Double.MaxValue)
    
    val first = abcMethod.init
    
    assert(first.length === 5)
    first.foreach(a => assert(a === expectedParticle))
  }
}