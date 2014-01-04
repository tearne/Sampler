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
		extends TestKit(ActorSystem("testSystem"))
		with FreeSpecLike
		with BeforeAndAfter
		with MockitoSugar {
	
	val oneSecond = 1000
	
	case class DullParams()
	
	class TestSubject(
			val model: Model[DullParams], 
			val config: ABCConfig
	) extends RootActor[DullParams]
			with AlgorithmComponent
			with ChildrenActorsComponent[DullParams]
			with GettersComponent {
		val childActors = mock[ChildActors]
		val algorithm = mock[Algorithm]
		override val getters = mock[Getters]
	}
	
	after {
		TestKit.shutdownActorSystem(system)
	}
	
	"Inisialisation should" - {
		"send job to work router then become 'Gathering'" in {
			val model = mock[Model[DullParams]]
			val config = mock[ABCConfig]
			
			val actorRef = TestFSMRef(
				new TestSubject(model, config)
			)
			val routerProbe = TestProbe()
			val clientProbs = TestProbe()
			
			val actorObject = actorRef.underlyingActor
			when(actorObject.childActors.workerRouter).thenReturn(routerProbe.ref)
			when(actorObject.getters.getMixRateMS(config)).thenReturn(oneSecond)
			
			val generation0 = Generation(null, null, null, 0, 0, Map[DullParams, Double]())
			
			actorRef.tell(Start(generation0), clientProbs.ref)
			
			routerProbe.expectMsg(
					Job(generation0.prevWeightsTable, config))
			assertResult(Gathering)(
					actorRef.stateName)
			assertResult(Progress(generation0, clientProbs.ref))(
					actorRef.stateData)
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