//package sampler.cluster.abc.actor.root
//
//import akka.testkit.TestActorRef
//import org.scalatest.mock.MockitoSugar
//import org.mockito.Mockito._
//import sampler.cluster.abc.parameters.ABCParameters
//import sampler.cluster.abc.state.StateEngineService
//import akka.testkit.TestKit
//import akka.actor.ActorSystem
//import org.scalatest.FreeSpecLike
//
//class RootActorTest 
//		extends TestKit(ActorSystem("testSystem"))
//		with FreeSpecLike 
//		with MockitoSugar {
//	
//	"Inisialisation should" - {
//		"send job to work router then become 'busy'" in {
//			val abcParams = mock[ABCParameters]
//			val stateEngine = mock[StateEngineService]
//			
//			val instanceRef = TestActorRef(new Root(
//					null,
//					abcParams,
//					null,
//					stateEngine
//			))
//			
//			
//		}
//	}
//}