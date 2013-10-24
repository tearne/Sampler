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
  
  implicit var r: Random = _
  
  var myModel: AlwaysOneModel = _
  
  var meta: ABCParameters = _
  
  @Before
  def setup {
    r = Random
    myModel = new AlwaysOneModel
    
    val reps = 1
    val particles = 2
    val tolerance = 1e6
    val refinements = 1
    val particleRetries = 1
    val chunkSize = 1
    
    meta = ABCParameters(
        reps, particles, tolerance, refinements, particleRetries, chunkSize)
  }
  
  @Test
  def testInitialisation {
    val initialModel = new AlwaysOneModel
    val parameters = mock[ABCParameters]
    when(parameters.numParticles).thenReturn(5)
    
    val abcMethod = new ABCMethod(parameters, r)
    
    val expectedParticle = Particle(initialModel.Parameters(1), 1.0, Double.MaxValue)
    
    val ePop0: EncapsulatedPopulation[initialModel.type] = abcMethod.init(initialModel)
    val pop0 = ePop0.population
    
    assert(pop0.length === 5)
    pop0.foreach(a => assert(a === expectedParticle))
  }
  
  @Test
  def runReturnsInitialPopluationWhenRefinementsIsZero {
	val popBuilder = mock[LocalPopulationBuilder]
    
	val noRefinementsMeta = mock[ABCParameters]
    when(noRefinementsMeta.numParticles).thenReturn(1)
    when(noRefinementsMeta.refinements).thenReturn(0)
    
    val abcMethod = new ABCMethod(noRefinementsMeta, r)
    
    val p0 = abcMethod.init(myModel)
    
    val p1 = abcMethod.run(p0, popBuilder).getOrElse(throw new AssertionError("No population returned"))
    
    assert(p1 === p0)
  }
  
  @Test def populationEvolvesSuccessfully{
  	val abcMethod = new ABCMethod(meta, r)
  
  	val popBuilder = mock[LocalPopulationBuilder]
  	val p0 = abcMethod.init(myModel)
  	val tolerance = 0.3
  	
  	val expectedParticle1 = Particle[p0.model.Parameters](p0.model.Parameters(1), 0.5, 400)
  	val expectedParticle2 = Particle[p0.model.Parameters](p0.model.Parameters(2), 0.5, 410)
  	
  	when(popBuilder.run(p0, Seq(1,1), tolerance, meta, r)).thenReturn(
  		Seq(
  			Success(EncapsulatedPopulation(p0.model)(Seq(expectedParticle1))),
  			Success(EncapsulatedPopulation(p0.model)(Seq(expectedParticle2)))
  		)
  	)
  	
  	val result = abcMethod.evolveOnce(p0, popBuilder, tolerance).get
  	
  	import result.model._
  	
  	assert(result.population === Seq(expectedParticle1,expectedParticle2))
  }
  
  @Test def evolveReturnsNoneIfHitMaxNumReps{
  	val abcMethod = new ABCMethod(meta, r)
  	
  	val popBuilder = mock[LocalPopulationBuilder]
  	val p0 = abcMethod.init(myModel)
  	val tolerance = 0.3
  	
  	val expectedParticle1 = Particle(p0.model.Parameters(1), 1.0, 400)
  	
  	when(popBuilder.run(p0, Seq(1,1), tolerance, meta, r)).thenReturn(
  		Seq(
  			Success(EncapsulatedPopulation(p0.model)(Seq(expectedParticle1))),
  			Failure(new RefinementAbortedException("Bleh"))
  		)
  	)
  	
  	val result = abcMethod.evolveOnce(p0, popBuilder, tolerance)
  	
  	assert(result === None)
  }

  @Test
  def evolveFailsIfAnyOtherExceptionReturned {
  	val abcMethod = new ABCMethod(meta, r)
  	
  	val popBuilder = mock[LocalPopulationBuilder]
  	val p0 = abcMethod.init(myModel)
  	val tolerance = 0.3
  	
  	when(popBuilder.run(p0, Seq(1,1), tolerance, meta, r)).thenReturn(
  		Seq(
  		    Failure(new RefinementAbortedException("Bleh")),
  			Failure(new ArrayIndexOutOfBoundsException("Bleh"))
  		)
  	)
  	
  	intercept[ArrayIndexOutOfBoundsException]{
    	abcMethod.evolveOnce(p0, popBuilder, tolerance)
    }
  }
  
  @Test
  def testCorrectChangingOfToleranceAsRefinementsGoOn {
  	fail("TODO")
//    TODO
  }
}