package sampler.abc

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.when
import org.mockito.Mockito.verify
import org.mockito.Matchers.anyObject
import sampler.math.Random
import sampler.abc.population.PopulationBuilder
import sampler.data.Distribution
import sampler.math.StatisticsComponent
import org.junit.Before
import sampler.abc.population.LocalPopulationBuilder
import sampler.abc.population.EncapsulatedPopulation

class ABCMethodTest extends AssertionsForJUnit {

  implicit var r: Random = _
  
  class AlwaysOneModel extends ABCModel with StatisticsComponent {
    case class Parameters(i: Int) extends ParametersBase with Serializable {
      def perturb() = Parameters(i)
      def perturbDensity(that: Parameters) = 1.0
    }

    val observations = Observations(2)

    case class Observations(n: Double) extends ObservationsBase with Serializable{}

    case class Output(simulated: Observations) extends OutputBase with Serializable{
      def distanceTo(obs: Observations): Double = simulated.n - obs.n
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
  
  @Before
  def setup {
    r = Random
  }
  
  @Test
  def testInitialisation {
    val myModel = new AlwaysOneModel
    
    val parameters = mock[ABCParameters]
    when(parameters.numParticles).thenReturn(5)
    
    val abcMethod = new ABCMethod(parameters, r)
    
    val expectedParticle = Particle(myModel.Parameters(1), 1.0, Double.MaxValue)
    
    val ePop0: EncapsulatedPopulation[myModel.type] = abcMethod.init(myModel)
    val pop0 = ePop0.population
    
    assert(pop0.length === 5)
    pop0.foreach(a => assert(a === expectedParticle))
  }
  
  @Test
  def runReturnsInitialPopluationWhenRefinementsIsZero {
    val myModel = new AlwaysOneModel
    
    val meta = mock[ABCParameters]
    val popBuilder = mock[PopulationBuilder]
    
    when(meta.numParticles).thenReturn(1)
    when(meta.refinements).thenReturn(0)
    
    val abcMethod = new ABCMethod(meta, r)
    
    val p0 = abcMethod.init(myModel)
    
    val p1 = abcMethod.run(p0, popBuilder).getOrElse(throw new AssertionError("No population returned"))
    
    assert(p1 === p0)
  }
  
  @Test
  def runOneRefinementWithCorrectArguments {
    val myModel = new AlwaysOneModel
    
    import myModel._
    val pop = mock[Population]
    
    val popBuilder = mock[LocalPopulationBuilder]
    
    val meta = ABCParameters(1, 1, 1e6, 1, 1, 1)
    
    val abcMethod = new ABCMethod(meta, r)
    
    val p0 = abcMethod.init(myModel)
    
    when(popBuilder.run(p0.model)(p0.population, List(1), 1e6, meta, r)).thenReturn(null)

    abcMethod.evolveOnce(p0, popBuilder, meta.tolerance)
  }
  
  @Test
  def testCollectionOfLeftAndRightsInEvolveOnce {
//    TODO
  }
  
  @Test
  def testCorrectChangingOfToleranceAsRefinementsGoOn {
//    TODO
  }
}