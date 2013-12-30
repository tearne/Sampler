//package sampler.cluster.abc.actor.root
//
//import akka.testkit.TestActorRef
//import org.scalatest.mock.MockitoSugar
//import org.mockito.Mockito._
//import sampler.cluster.abc.parameters.ABCParameters
//import akka.testkit.TestKit
//import akka.actor.ActorSystem
//import org.scalatest.FreeSpecLike
//import sampler.abc.ABCModel
//import sampler.math.Statistics
//import sampler.math.Random
//import sampler.cluster.abc.state.StateEngineComponent
//import sampler.cluster.abc.state.StateEngine
//import akka.testkit.TestActor
//import org.scalatest.BeforeAndAfter
//import sampler.cluster.abc.state.EncapsulatedState
//import sampler.abc.Prior
//import sampler.data.Distribution
//import sampler.cluster.abc.state.State
//import sampler.cluster.abc.actor.Start
//import sampler.cluster.abc.actor.Job
//import sampler.cluster.abc.parameters.ClusterParameters
//
//class RootActorTest 
//		extends TestKit(ActorSystem("testSystem"))
//		with FreeSpecLike
//		with BeforeAndAfter
//		with MockitoSugar {
//	
//	val oneSecond = 1000
//	
//	val mockModel = mock[ABCModel]
//	val mockStatistics = mock[Statistics]
//	val mockRandom = mock[Random]
//	val mockABCParams = mock[ABCParameters]
//	
//	class TestingRootActor(val abcParams: ABCParameters) 
//			extends RootActor 
//			with StateEngineComponent
//			with ChildrenActorsComponent {
//		val childrenActors = mock[ChildrenActors]
//		val stateEngine = mock[StateEngine]
//		val model = mockModel
//	}
//	
//	object TestModel extends ABCModel{
//		case class ParameterSet(i: Int) extends ParameterSetBase with Serializable {
//	      def perturb() = this
//	      def perturbDensity(that: ParameterSet) = if(that == this) 1.0 else 0.0
//	    }
//	
//	    case class Observed()
//	    val observed = Observed()
//	    
//	    case class Simulated() extends SimulatedBase{
//	      def distanceToObserved: Double = 1.0
//	    }
//	          
//	    def modelDistribution(p: ParameterSet) = new Distribution[Simulated] with Serializable {
//	      override def sample = Simulated()
//	    }
//	      
//	    val prior = new Prior[ParameterSet] with Serializable{
//	      val dist = Distribution.continually(1)
//	      def density(p: ParameterSet) = 1.0
//	      def sample() = ParameterSet(1)
//	    }
//	}
//	
//	after {
//		TestKit.shutdownActorSystem(system)
//	}
//	
//	"Inisialisation should" - {
//		"send job to work router then become 'busy'" in {
//			val abcParams = ABCParameters(
//					null,
//					null,
//					ClusterParameters(false, 0, 0, 0, 0, oneSecond)
//			)
//			val actorRef = TestActorRef(new TestingRootActor(mockABCParams))
//			val actor = actorRef.underlyingActor
//			when(actor.childrenActors.workerRouter).thenReturn(testActor)
//			
//			val particleTable = mock[Map[TestModel.ParameterSet, Double]]
//			val state = State(null, null, null, 0, 0, particleTable)
//			val eState = EncapsulatedState(TestModel)(state)
//			
//			actorRef ! Start(eState)
//			
//			expectMsg(Job(particleTable, mockABCParams))
//		}
//	}
//}