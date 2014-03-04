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
import sampler.cluster.abc.actor.ScoredParticles
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.Scored
import sampler.cluster.abc.Weighted
import sampler.cluster.abc.config.ClusterParameters
import sampler.cluster.abc.config.JobParameters
import sampler.cluster.abc.actor.Report
import sampler.cluster.abc.actor.GenerateJob
import akka.routing.Broadcast
import sampler.cluster.abc.actor.WeighJob
import sampler.cluster.abc.actor.ScoredParticles
import sampler.cluster.abc.actor.Failed
import sampler.cluster.abc.actor.GenerateJob
		
@RunWith(classOf[JUnitRunner])
class ABCActorTest 
		extends TestKit(ActorSystem("ABC"))
		with FreeSpecLike
		with BeforeAndAfterAll
		with MockitoSugar {
	
	val oneSecond = 1000l
	val hundredParticles = 100
	val fiveGenerations = 5
	val isFinal = true
	val terminateAtTargetGen = true
	
	case class DullParams()
	
	class ConcreteABCActor(
			val model: Model[DullParams], 
			val config: ABCConfig,
			val reportAction: Option[Report[DullParams] => Unit],
			override val getters: Getters
	) extends ABCActor[DullParams]
			with AlgorithmComponent
			with ChildrenActorsComponent[DullParams] 
			with WorkDispatcherComponent
			with GettersComponent {
		val childActors = mock[ChildActors]
		val algorithm = mock[Algorithm]
		val workDispatcher = context.dispatcher
	}
	
	override def afterAll {
		TestKit.shutdownActorSystem(system)
	}
	
	def getInstance = {
		val model = mock[Model[DullParams]]
		val config = ABCConfig(
				JobParameters(hundredParticles, 0, fiveGenerations),
				null,
				ClusterParameters(terminateAtTargetGen, 0, 0l, 0, oneSecond, 0l)
		)
		val reportAction = None
		val getters = mock[Getters]; when(getters.getMixRateMS(config)).thenReturn(oneSecond)
			
		TestFSMRef(new ConcreteABCActor(model, config, reportAction, getters))
	}
	
	"Mixing test" - {
	  //TODO add test for reporting actions
	  
	  "When Gathering and Failed generate a new job" in {
		  val instanceRef = getInstance
		  val instanceObj = instanceRef.underlyingActor
				  
		  // Setup
		  val routerProbe = TestProbe()
		  val clientProbe = TestProbe()
		  val prevWeights = Map[DullParams, Double]()
		  val gen0 = Generation(null, ScoredParticles(Seq()), null, null, 0, 0, prevWeights)
		  when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)
		
		  val gen1 = mock[Generation[DullParams]]
		  
		  when(instanceObj.algorithm.emptyWeighingBuffer(gen0)
		  ).thenReturn(gen1)
		  
		  val failed = Failed
				  
		  // Action
		  instanceRef tell(Start(gen0), clientProbe.ref)
		  assertResult(Gathering)(instanceRef.stateName)
		  
		  instanceRef tell(failed, clientProbe.ref)
		  
		  // Assertion
		  routerProbe.expectMsg(Broadcast(GenerateJob(prevWeights, instanceObj.config)))
		  assertResult(Gathering)(instanceRef.stateName)
		  assertResult(gen1)(instanceRef.stateData match {
		  	case gd: StateData[_] => gd.generation
		  	case d => fail("Unexpected StateData type: "+d.getClass())
		  })
	  }
	  
	  "When Gathering and Failed with particles due weighing instruct to weigh" in {
		  val instanceRef = getInstance
		  val instanceObj = instanceRef.underlyingActor
				  
		  // Setup
		  val routerProbe = TestProbe()
		  val clientProbe = TestProbe()
		  val dueWeighing = mock[ScoredParticles[DullParams]]
		  when(dueWeighing.size).thenReturn(10)
		  val prevWeights = Map[DullParams, Double]()
		  val gen0 = Generation(null, dueWeighing, null, null, 0, 0, prevWeights)
		  when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)
			
		  val failed = Failed
				  
		  // Action
		  instanceRef tell(Start(gen0), clientProbe.ref)
		  assertResult(Gathering)(instanceRef.stateName)
		  
		  instanceRef tell(failed, clientProbe.ref)
		  
		  // Assertion
		  routerProbe.expectMsg(Broadcast(WeighJob(dueWeighing, prevWeights, 0)))
		  assertResult(Gathering)(instanceRef.stateName)
		  assertResult(gen0)(instanceRef.stateData match {
		  	case gd: StateData[_] => gd.generation
		  	case d => fail("Unexpected StateData type: "+d.getClass())
		  })
	  }
	}
	
	"Lifecycle test" in {
		val instanceRef = getInstance
		val instanceObj = instanceRef.underlyingActor

		/*
		 *  Start (Idle -> Gathering)
		 */
		// Setup
		val routerProbe = TestProbe()
		val clientProbe = TestProbe()
		val gen0 = Generation(null, null, null, null, 0, 0, Map[DullParams, Double]())
		when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)
		
		// Action
		instanceRef tell(Start(gen0), clientProbe.ref)
		
		// Tell doesn't expect response (fire and forget)
		// Ask returns handle to answer which will be completed
		
		// Assertions
		routerProbe.expectMsg(Broadcast(GenerateJob(gen0.prevWeightsTable, instanceObj.config)))
		assertResult(Gathering)(instanceRef.stateName)
		assertResult(gen0)(instanceRef.stateData match {
			case gd: StateData[_] => gd.generation
			case d => fail("Unexpected StateData type: "+d.getClass())
		})
		
		
		/*
		 * Send payload from worker to instance, and expect to be asked to weigh particles
		 */ 
		// Setup
		val workerProbe = TestProbe()
		val dueWeighing = ScoredParticles(Seq.empty[Tagged[Scored[DullParams]]])
		val prevWeightsTable = Map[DullParams, Double]()
		val currentTolerance = 25
		val gen1 = Generation(null, dueWeighing, null, null, currentTolerance, 0, prevWeightsTable)
		val newScoredParticles = ScoredParticles(Seq.empty[Tagged[Scored[DullParams]]])
		when(instanceObj.algorithm.filterAndQueueForWeighing(
				newScoredParticles,
				gen0
		)).thenReturn(gen1)
		
		// Action
		instanceRef tell(newScoredParticles, workerProbe.ref)
		
		// Assertions
		workerProbe.expectMsg(WeighJob(dueWeighing, prevWeightsTable, currentTolerance))
		assertResult(Gathering)(instanceRef.stateName)
		val stateData1 = instanceRef.stateData.asInstanceOf[StateData[DullParams]]
		assertResult(gen1)(stateData1.generation)
		assertResult(clientProbe.ref)(stateData1.client)
//		
//		
//		/*
//		 * Payload which triggers generation flushing
//		 */
//		// Setup
//		val gen2 = Generation(null, null, null, 0, 0, Map[DullParams, Double]())
//		val newParticles2 = Seq.empty[Tagged[Scored[DullParams]]]
//		when(instanceObj.algorithm.add(
//				gen1, 
//				newParticles2, 
//				workerProbe.ref, 
//				instanceObj.config
//		)).thenReturn(gen2)
//		when(instanceObj.algorithm.numberAccumulated(gen2)).thenReturn(hundredParticles)
//		val gen3 = Generation(null, null, null, 0, 0, Map[DullParams, Double]())
//		when(instanceObj.algorithm.flushGeneration(gen2, hundredParticles)).thenReturn(gen3)
//		val report1 = mock[Report[DullParams]]
//		when(instanceObj.algorithm.buildReport(gen3, instanceObj.config, !isFinal)).thenReturn(report1)
//		val newDataMsg2 = TaggedScoreSeq[DullParams](newParticles2)
//		
//		// Action
//		instanceRef tell(newDataMsg2, workerProbe.ref)
//		
//		// Assertions (back to gathering when flush complete)
//		assertResult(Gathering)(instanceRef.stateName)
//		val stateData2 = instanceRef.stateData.asInstanceOf[GatheringData[DullParams]]
//		assertResult(gen3)(stateData2.generation)
//		assertResult(clientProbe.ref)(stateData2.client)
//		clientProbe.expectMsg(report1)	//Evidence that the flush took place
//		
//		/*
//		 * Payload which drives generation buffer above the required
//		 * number of particles and finishes the final generation
//		 */
//		// Setup
//		val gen4 = Generation(null, null, null, 0, 0, Map[DullParams, Double]())
//		val newParticles3 = Seq.empty[Tagged[Scored[DullParams]]]
//		when(instanceObj.algorithm.add(
//				gen3, 
//				newParticles3, 
//				workerProbe.ref, 
//				instanceObj.config
//		)).thenReturn(gen4)
//		when(instanceObj.algorithm.numberAccumulated(gen4)).thenReturn(hundredParticles + 1)
//		val gen5 = Generation(null, null, null, 0, fiveGenerations, Map[DullParams, Double]())
//		when(instanceObj.algorithm.flushGeneration(gen4, hundredParticles)).thenReturn(gen5)
//		val report2 = mock[Report[DullParams]]
//		when(instanceObj.algorithm.buildReport(gen5, instanceObj.config, isFinal)).thenReturn(report2)
//		val newDataMsg3 = TaggedScoreSeq[DullParams](newParticles2)
//		
//		// Action
//		instanceRef tell(newDataMsg3, workerProbe.ref)
//		
//		// Assertions
//		assertResult(Idle)(instanceRef.stateName)
//		assertResult(Uninitialized)(instanceRef.stateData)
//		clientProbe.expectMsg(report2)
	}
}