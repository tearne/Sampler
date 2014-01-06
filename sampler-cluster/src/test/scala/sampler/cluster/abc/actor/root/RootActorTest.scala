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
import org.scalatest.BeforeAndAfterAll
import sampler.cluster.abc.actor.TaggedScoreSeq
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.Scored
import sampler.cluster.abc.Weighted
import sampler.cluster.abc.config.ClusterParameters
import sampler.cluster.abc.config.JobParameters
		
@RunWith(classOf[JUnitRunner])
class RootActorTest 
		extends TestKit(ActorSystem("ABC"))
		with FreeSpecLike
		with BeforeAndAfterAll
		with MockitoSugar {
	
	val oneSecond = 1000l
	val fiveParticles = 5
	
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
	
	override def afterAll {
		TestKit.shutdownActorSystem(system)
	}
	
	def getInstance = {
		val model = mock[Model[DullParams]]
		val config = ABCConfig(
				JobParameters(fiveParticles, 0, 0),
				null,
				ClusterParameters(false, 0, 0l, 0, oneSecond, 0l)
		)
		val getters = mock[Getters]; when(getters.getMixRateMS(config)).thenReturn(oneSecond)
			
		TestFSMRef(new TestSubject(model, config, getters))
	}
	
	"RootActor should" - {
		"start 'Idle'" in {
			assertResult(Idle)(getInstance.stateName)
		}
		"send initial job to work router and start gathering" in {
			val instanceRef = getInstance
			val instanceObj = instanceRef.underlyingActor
			
			val routerProbe = TestProbe()
			val clientProbe = TestProbe()
			val generation0 = Generation(null, null, null, 0, 0, Map[DullParams, Double]())
			
			when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)
			
			instanceRef.tell(Start(generation0), clientProbe.ref)
			
			routerProbe.expectMsg(Job(generation0.prevWeightsTable, instanceObj.config))
			assertResult(Gathering)(instanceRef.stateName)
			assertResult(generation0)(instanceRef.stateData match {
				case gd: GatheringData[_] => gd.generation
				case d => fail("Unexpected StateData type: "+d.getClass())
			})
		}
		"stay gathering if not enough particles" in {
			val instanceRef = getInstance
			val instanceObj = instanceRef.underlyingActor

			/*
			 *  Setup (Idle -> Gathering)
			 */
			val clientProbe = TestProbe()
			val gen0 = Generation(null, null, null, 0, 0, Map[DullParams, Double]())
			when(instanceObj.childActors.workerRouter).thenReturn(TestProbe().ref)
			instanceRef tell(Start(gen0), clientProbe.ref)
			
			/*
			 * Send payload from worker to instance
			 */ 
			val workerProbe = TestProbe()
			val genUpdated = mock[Generation[DullParams]]
			val newParticles = Seq.empty[Tagged[Scored[DullParams]]]
			when(instanceObj.algorithm.add(
					gen0, 
					newParticles, 
					workerProbe.ref, 
					instanceObj.config
			)).thenReturn(genUpdated)
			// Still more to gather before finalising the generation
			when(instanceObj.algorithm.numberAccumulated(genUpdated)).thenReturn(2)
			val newDataMsg = TaggedScoreSeq[DullParams](newParticles)
			
			instanceRef tell(newDataMsg, workerProbe.ref)
			
			assertResult(Gathering)(instanceRef.stateName)
			val stateData = instanceRef.stateData.asInstanceOf[GatheringData[DullParams]]
			assertResult(genUpdated)(stateData.generation)
			assertResult(clientProbe.ref)(stateData.client)
		}
		"flush new generation when enough particles" in {
			val instanceRef = getInstance
			val instanceObj = instanceRef.underlyingActor

			/*
			 *  Setup (Idle -> Gathering)
			 */
			val clientProbe = TestProbe()
			val gen0 = Generation(null, null, null, 0, 0, Map[DullParams, Double]())
			when(instanceObj.childActors.workerRouter).thenReturn(TestProbe().ref)
			instanceRef tell(Start(gen0), clientProbe.ref)
			
			/*
			 * Send payload from worker to instance
			 */ 
			val workerProbe = TestProbe()
			val genUpdated = mock[Generation[DullParams]]
			val newParticles = Seq.empty[Tagged[Scored[DullParams]]]
			when(instanceObj.algorithm.add(
					gen0, 
					newParticles, 
					workerProbe.ref, 
					instanceObj.config
			)).thenReturn(genUpdated)
			// Pretend we've collected enough particles to flush
			when(instanceObj.algorithm.numberAccumulated(genUpdated)).thenReturn(6)
			val genFlushed = mock[Generation[DullParams]]
			when(instanceObj.algorithm.flushGeneration(genUpdated, fiveParticles)).thenReturn(genFlushed)
			val newDataMsg = TaggedScoreSeq[DullParams](newParticles)
			
			
			// todo finish ... need to handle the flushing stuff
			
			instanceRef tell(newDataMsg, workerProbe.ref)
//			
//			assertResult(Gathering)(instanceRef.stateName)
//			val stateData = instanceRef.stateData.asInstanceOf[GatheringData[DullParams]]
//			assertResult(genUpdated)(stateData.generation)
//			assertResult(clientProbe.ref)(stateData.client)
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