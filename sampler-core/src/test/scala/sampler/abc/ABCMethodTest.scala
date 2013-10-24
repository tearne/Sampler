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
import scala.util.Success
import scala.util.Failure

class ABCMethodTest extends AssertionsForJUnit {

  implicit var r: Random = _
  
  class AlwaysOneModel extends ABCModel with StatisticsComponent {
    case class Parameters(i: Int) extends ParametersBase with Serializable {
      def perturb() = this//Parameters(i)
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
  
  @Test def populationEvolvedSuccessfully{
  	val myModel = new AlwaysOneModel
  	val meta = ABCParameters(1, 4, 1e6, 1, 1, 2)
  	val abcMethod = new ABCMethod(meta, r)
  	
  	val popBuilder = mock[LocalPopulationBuilder]
  	val p0 = abcMethod.init(myModel)
  	val tolerance = 0.3
  	
  	val expectedParticle1 = Particle[p0.model.Parameters](p0.model.Parameters(1), 0.3, 400)
  	val expectedParticle2 = Particle[p0.model.Parameters](p0.model.Parameters(2), 0.4, 410)
  	val expectedParticle3 = Particle[p0.model.Parameters](p0.model.Parameters(3), 0.5, 420)
  	val expectedParticle4 = Particle[p0.model.Parameters](p0.model.Parameters(4), 0.6, 430)
  	
  	when(popBuilder.run(p0, Seq(2,2), tolerance, meta, r)).thenReturn(
  		Seq(
  			Success(EncapsulatedPopulation(p0.model)(Seq(expectedParticle1, expectedParticle2))),
  			Success(EncapsulatedPopulation(p0.model)(Seq(expectedParticle3, expectedParticle4)))
  		)
  	)
  	
  	val result = abcMethod.evolveOnce(p0, popBuilder, tolerance).get
  	
  	import result.model._
  	
  	assert(result.population === Seq(expectedParticle1,expectedParticle2,expectedParticle3,expectedParticle4))
  }
  
  @Test def noneIfHitMaxNumReps{
  	val myModel = new AlwaysOneModel
  	val meta = ABCParameters(1, 4, 1e6, 1, 1, 2)
  	val abcMethod = new ABCMethod(meta, r)
  	
  	val popBuilder = mock[LocalPopulationBuilder]
  	val p0 = abcMethod.init(myModel)
  	val tolerance = 0.3
  	
  	val expectedParticle1 = Particle(p0.model.Parameters(1), 0.3, 400)
  	val expectedParticle2 = Particle(p0.model.Parameters(2), 0.4, 410)
  	
  	when(popBuilder.run(p0, Seq(2,2), tolerance, meta, r)).thenReturn(
  		Seq(
  			Success(EncapsulatedPopulation(p0.model)(Seq(expectedParticle1, expectedParticle2))),
  			Failure(new RefinementAbortedException("Bleh"))
  		)
  	)
  	
  	val result = abcMethod.evolveOnce(p0, popBuilder, tolerance)
  	
  	assert(result === None)
  }
  
  @Test
  def testCollectionOfLeftAndRightsInEvolveOnce {
  	fail("TODO")
//    TODO
  }
  
  @Test
  def testCorrectChangingOfToleranceAsRefinementsGoOn {
  	fail("TODO")
//    TODO
  }
}