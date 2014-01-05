package sampler.cluster.abc.actor.root

import org.mockito.Mockito.when
import org.scalatest.BeforeAndAfter
import org.scalatest.FreeSpecLike
import org.scalatest.mock.MockitoSugar
import akka.actor.ActorSystem
import akka.testkit.TestFSMRef
import akka.testkit.TestKit
import akka.testkit.TestProbe
import sampler.cluster.abc.Model
import sampler.cluster.abc.actor.Job
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.algorithm.Algorithm
import sampler.cluster.abc.algorithm.AlgorithmComponent
import sampler.cluster.abc.algorithm.Generation
import sampler.cluster.abc.config.ABCConfig
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RootActorTest 
		extends TestKit(ActorSystem("ABC"))
		with FreeSpecLike
		with BeforeAndAfter
		with MockitoSugar {
	
	val oneSecond = 1000l
	
	case class DullParams()
	
	class TestSubject(
			val model: Model[DullParams], 
			val config: ABCConfig,
			override val getters: Getters
	) extends RootActor[DullParams]
			with AlgorithmComponent
			with ChildrenActorsComponent[DullParams] 
			with GettersComponent {
		val childActors = mock[ChildActors]
		val algorithm = mock[Algorithm]		
	}
	
	after {
		TestKit.shutdownActorSystem(system)
	}
	
	"RootActor should" - {
		"start 'Idle'" in pending
		"send job to work router then become 'Gathering' upon Start msg" in {
			val model = mock[Model[DullParams]]
			val config = mock[ABCConfig]
			val getters = mock[Getters]; when(getters.getMixRateMS(config)).thenReturn(oneSecond)
			
			val instanceRef = TestFSMRef(new TestSubject(model, config, getters))
			
			val routerProbe = TestProbe()
			val clientProbs = TestProbe()
			
			val instanceObject = instanceRef.underlyingActor
			when(instanceObject.childActors.workerRouter).thenReturn(routerProbe.ref)
			
			val generation0 = Generation(null, null, null, 0, 0, Map[DullParams, Double]())
			
			instanceRef.tell(Start(generation0), clientProbs.ref)
			
			routerProbe.expectMsg(Job(generation0.prevWeightsTable, config))
			assertResult(Gathering)(instanceRef.stateName)
			assertResult(generation0)(instanceRef.stateData match {
				case gd: GatheringData[_] => gd.generation
				case d => fail("Unexpected StateData type: "+d.getClass())
			})
		}
	}
	"Lifecycle test" in {
		pending
//		assertResult(Idle)(actorRef.stateName)
//		assertResult(Gathering)(actorRef.stateName)
//		assertResult(Gathering)(actorRef.stateName)
//		assertResult(Flushing)(actorRef.stateName)
//		assertResult(Gathering)(actorRef.stateName)
//		assertResult(Flushing)(actorRef.stateName)
//		assertResult(Idle)(actorRef.stateName)
	}
}