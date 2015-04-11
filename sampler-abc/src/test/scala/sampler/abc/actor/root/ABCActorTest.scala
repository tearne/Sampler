package sampler.abc.actor.root

import org.junit.runner.RunWith
import org.mockito.Mockito.when
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FreeSpecLike
import org.scalatest.mock.MockitoSugar
import akka.actor.ActorSystem
import akka.routing.Broadcast
import akka.testkit.TestFSMRef
import akka.testkit.TestKit
import akka.testkit.TestProbe
import sampler.abc.Model
import sampler.abc.Scored
import sampler.abc.actor.Failed
import sampler.abc.actor.Report
import sampler.abc.actor.ScoredParticles
import sampler.abc.actor.Start
import sampler.abc.actor.Tagged
import sampler.abc.actor.WeighJob
import sampler.abc.config.ABCConfig
import sampler.abc.config.ClusterParameters
import sampler.abc.config.JobParameters
import sampler.abc.actor.MixPayload
import sampler.abc.actor.WeighedParticles
import sampler.abc.actor.Abort
import sampler.abc.actor.ReportCompleted
import sampler.abc.actor.ReportCompleted
import akka.actor.Cancellable
import sampler.abc.actor.ScoredParticles
import sampler.abc.actor.algorithm.Getters
import sampler.abc.core.Generation
import sampler.abc.actor.algorithm.Algorithm
import sampler.abc.actor.GenerateParticles
import sampler.abc.actor.algorithm.EvolvingGeneration
import sampler.abc.core.Reporter

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
	
	class TestableABCActor(
			val model: Model[DullParams], 
			val config: ABCConfig,
			val reportAction: Option[Report[DullParams] => Unit],
			override val getters: Getters
		) extends ABCActor[DullParams]
			with ChildrenActorsComponent[DullParams] 
			with WorkDispatcherComponent {
		val childActors = mock[ChildActors]
		val algorithm = mock[Algorithm]
		val workDispatcher = context.dispatcher
		
		implicit val distributionBuilder =  sampler.data.DistributionBuilder
		implicit val random = sampler.math.Random
		val reporter = mock[Reporter]
	}
	
	override def afterAll {
		TestKit.shutdownActorSystem(system)
	}
	
	//TODO use instance setup trait
	trait Instance {
		val model = mock[Model[DullParams]]
		val config = ABCConfig(
				JobParameters(hundredParticles, 0, fiveGenerations),
				null,
				ClusterParameters(terminateAtTargetGen, 0, 0l, 0, oneSecond, 0l)
		)
		val reportAction = None
		val getters = mock[Getters]
		when(getters.getMixRateMS(config)).thenReturn(oneSecond)
			
		val instanceRef = TestFSMRef(new TestableABCActor(model, config, reportAction, getters))
	}
	
	"When Idle" - {

	  "recieving Start message sends broadcast and sets internal state" in new Instance {
			val routerProbe = TestProbe()
			val clientProbe = TestProbe()	//TODO really need client probe?
				  
			val instanceObj = instanceRef.underlyingActor
			when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
			
			val particleWeights = Map[DullParams, Double]()
	    
			val gen0 = Generation(null, 0, particleWeights, 0)
	    
			// Action
			instanceRef tell(Start(gen0), clientProbe.ref)
		
			// Assertions
			routerProbe.expectMsg(Broadcast(GenerateParticles(particleWeights, instanceObj.config)))
			assertResult(Gathering)(instanceRef.stateName)
			assertResult(gen0)(instanceRef.stateData match {
				case gd: StateData[_] => gd.generation
				case d => fail("Unexpected StateData type: "+d.getClass())
			})
	  }
	}
	
	"When Gathering" - {
	  "and recieve Failed message from Worker" - {
//	    val failed = Failed
	    val prevWeights = Map[DullParams, Double]()
//	    val gen0 = Generation(null, 0, prevWeights, 0)
	    
	    "if no weighing jobs waiting then tells worker to start new job" in new Instance{
	    	val workerProbe = TestProbe()
		  
		  	val instanceObj = instanceRef.underlyingActor
		  
		    val dueWeighing = mock[ScoredParticles[DullParams]]
		  	when(dueWeighing.size).thenReturn(0)
	    	val eGen = EvolvingGeneration(0, null, dueWeighing, null, null)
		  	
		  	instanceRef.setState(Gathering, StateData(eGen, null, None))
		  
		  	// Action
		  	instanceRef tell(Failed, workerProbe.ref)
		  
		  	// Assertion
		  	workerProbe.expectMsg(GenerateParticles(prevWeights, instanceObj.config))
		  
		  	assertResult(Gathering)(instanceRef.stateName)
		  	assertResult(eGen)(instanceRef.stateData match {
			  	case gd: StateData[_] => gd.generation
			  	case d => fail("Unexpected StateData type: "+d.getClass())
			  })
		  }
	  
	    "if particles waiting to be weighed tells worker to weigh" in new Instance{
	      val workerProbe = TestProbe()

	      val instanceObj = instanceRef.underlyingActor

	      val dueWeighing = mock[ScoredParticles[DullParams]]
	      when(dueWeighing.size).thenReturn(10)
	    	
	      val eGen0 = EvolvingGeneration(0, null, dueWeighing, null, null)
	      val eGen1 = mock[EvolvingGeneration[DullParams]]

	      instanceRef.setState(Gathering, StateData(eGen0, null, None))
	    	
	      val algorithm = instanceObj.algorithm
	      when(algorithm.emptyWeighingBuffer(eGen0)).thenReturn(eGen1)
	    	
	      // Action
	      instanceRef tell(Failed, workerProbe.ref)

	      // Assertion
	      workerProbe.expectMsg(WeighJob(dueWeighing, prevWeights, 0))

	      assertResult(Gathering)(instanceRef.stateName)
	      assertResult(eGen1)(instanceRef.stateData match {
	        case gd: StateData[_] => gd.generation
	        case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	    }
	  }
	  
	  "when worker sends new scored particles it then gets a weighing job" in new Instance{
	    val clientProbe = TestProbe() //TODO do we need this?
	    val workerProbe = TestProbe()
	  
		  val instanceObj = instanceRef.underlyingActor
				  
		  val eGen0 = EvolvingGeneration(0, null, dueWeighing, null, null)
				  
		  instanceRef.setState(Gathering, StateData(eGen0, clientProbe.ref, None))
				  
		  val dueWeighing = ScoredParticles(Seq.empty[Tagged[Scored[DullParams]]])
		  val prevWeightsTable = Map[DullParams, Double]()
		  val oldTolerance = 30
		  val currentTolerance = 25
		  
		  val prevCompletedGen = Generation(null, 0, prevWeightsTable, oldTolerance)
		  val eGen1 = EvolvingGeneration(currentTolerance, prevCompletedGen, dueWeighing, null, null)
		  
		  val newScoredParticles = ScoredParticles(Seq.empty[Tagged[Scored[DullParams]]])
		  val wrongScoredParticles = mock[ScoredParticles[DullParams]]
	    
		  when(instanceObj.algorithm.filterAndQueueUnweighedParticles(
				wrongScoredParticles,//TODO UNDO SABOTAGE!
				eGen0
		  )).thenReturn(eGen1)
						  
		  // Action
		  instanceRef tell(newScoredParticles, workerProbe.ref)
		  
		  // Assertions
		  workerProbe.expectMsg(WeighJob(dueWeighing, prevWeightsTable, currentTolerance))
		  
		  assertResult(Gathering)(instanceRef.stateName)
		  val stateData1 = instanceRef.stateData.asInstanceOf[StateData[DullParams]]
		  assertResult(eGen1)(stateData1.generation)
		  assertResult(clientProbe.ref)(stateData1.client)
	  }
	  
	  "filters and queue particles from a MixPayload" in new Instance{
	    val clientProbe = TestProbe()
	    val routerProbe = TestProbe()
	    val workerProbe = TestProbe()
	    
			val instanceObj = instanceRef.underlyingActor
			when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	    
			val gen0 = mock[EvolvingGeneration[DullParams]]
			val gen1 = mock[EvolvingGeneration[DullParams]]
	    
	    when(gen1.dueWeighing).thenReturn(ScoredParticles(Seq.empty[Tagged[Scored[DullParams]]]))
	    
	    val payload = mock[MixPayload[DullParams]]
	    val scored = mock[ScoredParticles[DullParams]]
	    when(scored.seq).thenReturn(Seq())
	    when(payload.tss).thenReturn(scored)
	    
	    val algorithm = instanceObj.algorithm
	    when(algorithm.filterAndQueueUnweighedParticles(scored, gen0)).thenReturn(gen1)
		
	    instanceRef.setState(Gathering, StateData(gen0, clientProbe.ref, None))
	    
	    // Action
	    instanceRef tell(payload, clientProbe.ref)
	    
	    // Assert
	    assertResult(Gathering)(instanceRef.stateName)
	    assertResult(gen1)(instanceRef.stateData match {
	    	case gd: StateData[_] => gd.generation
	    	case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	  }
	  
	  "and more weighed particles arrive" - {
	    val newlyWeighted = mock[WeighedParticles[DullParams]]
	    
//	    val gen0 = mock[Generation[DullParams]]
//	    val gen1 = mock[Generation[DullParams]]
	    val eGen0 = mock[EvolvingGeneration[DullParams]]
	    val eGen1 = mock[EvolvingGeneration[DullParams]]
	    val eGen2 = mock[EvolvingGeneration[DullParams]]
	    
	    val prevWeights = Map[DullParams, Double]()
	    
	    "generate more particles if generation is incomplete" in new Instance{
	    	when(getters.getPreviousWeightsTable(eGen1)).thenReturn(prevWeights)
	      val workerProbe = TestProbe()
	      val stateData0 = StateData(eGen0, null, None)
	   
	      val instanceObj = instanceRef.underlyingActor
	      
	      val algorithm = instanceObj.algorithm
	      when(algorithm.addWeightedParticles(newlyWeighted, eGen0)).thenReturn(eGen1)
	      when(algorithm.isEnoughParticles(eGen1, config)).thenReturn(false)
	      
	      val dueWeighing = mock[ScoredParticles[DullParams]]
	      when(dueWeighing.size).thenReturn(100)
	      when(eGen1.dueWeighing).thenReturn(dueWeighing)
	      
	      instanceRef.setState(Gathering, stateData0)
	      
	      // Action
	      instanceRef tell(newlyWeighted, workerProbe.ref)
	      
	      // Assertion
	      workerProbe.expectMsg(GenerateParticles(prevWeights, config))
	      
	      assertResult(Gathering)(instanceRef.stateName)
	      assertResult(eGen1)(instanceRef.stateData match {
		    	case gd: StateData[_] => gd.generation
		    	case d => fail("Unexpected StateData type: "+d.getClass())
		    })
	    }
	    
	    "start new generation if got enough particles but need more generations" in new Instance{
	      val routerProbe = TestProbe()
	      val workerProbe = TestProbe()
	      val reportingProbe = TestProbe()
	      
	      val stateData0 = StateData(eGen0, null, None)
	   
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	      when(instanceObj.childActors.reportingActor).thenReturn(reportingProbe.ref)
	      
	      val algorithm = instanceObj.algorithm
	      when(getters.getPreviousWeightsTable(eGen1)).thenReturn(prevWeights)
	      when(algorithm.addWeightedParticles(newlyWeighted, eGen0)).thenReturn(eGen1)
	      
	      when(algorithm.isEnoughParticles(eGen1, config)).thenReturn(true)
	      
	      val flushedWeightsTable = Map[DullParams, Double](DullParams() -> 0.5, DullParams() -> 0.5)
	      
//	      val flushedGen = mock[Generation[DullParams]]
//	      when(flushedGen.currentIteration).thenReturn(1)
//	      when(flushedGen.currentTolerance).thenReturn(100)
//	      when(flushedGen.prevWeightsTable).thenReturn(flushedWeightsTable)
	      
	      when(algorithm.flushGeneration(eGen1)).thenReturn(eGen2)
	      
	      val report = mock[Report[DullParams]]	
	      val completedGeneration = mock[Generation[DullParams]]
	      when(eGen2.previousGen).thenReturn(completedGeneration)
	      when(instanceObj.reporter.build(completedGeneration)).thenReturn(report)
	      
	      instanceRef.setState(Gathering, stateData0)
	      
	      // Action
	      instanceRef tell(newlyWeighted, workerProbe.ref)
	      
	      // Expectations
	      routerProbe.expectMsg(Broadcast(Abort))
	      
	      routerProbe.expectMsg(Broadcast(GenerateParticles(flushedWeightsTable, instanceObj.config)))
	      
	      fail("TODO: where do we assert the report was sent?")
	      
	      assertResult(Gathering)(instanceRef.stateName)
	      assertResult(eGen2)(instanceRef.stateData match {
	    		case gd: StateData[_] => gd.generation
	    		case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	    }
	    
	    // TODO fix failing test
	    "If gathered enough particles and completed target generation then stop particle creation and await shutdown" in new Instance{
	      val routerProbe = TestProbe()
	      val workerProbe = TestProbe()
	      val reportingProbe = TestProbe()
	      
	      val stateData0 = StateData(eGen0, null, None)
	   
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	      when(instanceObj.childActors.reportingActor).thenReturn(reportingProbe.ref)
	      
	      val algorithm = instanceObj.algorithm
	      when(algorithm.addWeightedParticles(newlyWeighted, eGen0)).thenReturn(eGen1)
	      
	      when(algorithm.isEnoughParticles(eGen1, instanceObj.config)).thenReturn(true)
	      
	      val completedGen = mock[Generation[DullParams]]
	      when(completedGen.iteration).thenReturn(fiveGenerations)	// Matches config in setup above
	      when(completedGen.tolerance).thenReturn(hundredParticles)// Matches config in setup above
	      when(eGen1.previousGen).thenReturn(completedGen)
	      
	      when(algorithm.flushGeneration(eGen0)).thenReturn(eGen1)
	      
	      val report = mock[Report[DullParams]]	
	      when(instanceObj.reporter.build(completedGen)).thenReturn(report)
	      	
	      instanceRef.setState(Gathering, stateData0)
	      
	      // Action
	      instanceRef tell(newlyWeighted, workerProbe.ref)
	      
	      // Expectations
	      routerProbe.expectMsg(Broadcast(Abort))
	      
	      fail("TODO: where do we assert the report was sent?")
	      
	      assertResult(WaitingForShutdown)(instanceRef.stateName)
	      assertResult(eGen1)(instanceRef.stateData match {
	    		case gd: StateData[_] => gd.generation
	    		case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	    }
	  }
	  
	  "Builds a new payload and sends message to mix it in when instructed to mix now" in new Instance{
	    val broadcasterProbe = TestProbe()
	    val workerProbe = TestProbe()
	    
	    val instanceObj = instanceRef.underlyingActor
	    when(instanceObj.childActors.broadcaster).thenReturn(broadcasterProbe.ref)
	    
	    val eGen0 = mock[EvolvingGeneration[DullParams]]
	    
	    val scoredParticles = ScoredParticles(Seq(Tagged(Scored(DullParams(), Seq(1.0)), 100)))
	    val mixPayload = Some(scoredParticles)
	    
	    when(instanceObj.algorithm.buildMixPayload(eGen0, instanceObj.config)).thenReturn(mixPayload)
	    
	    // TODO Factor out to common?
	    val stateData = StateData(eGen0, null, None)
	    instanceRef.setState(Gathering, stateData)
	    
	    val mixNow = instanceObj.MixNow
	    
	    // Action
	    instanceRef tell(mixNow, workerProbe.ref)
	    
	    // Assertions
	    broadcasterProbe.expectMsg(MixPayload(scoredParticles))
	    
	    assertResult(Gathering)(instanceRef.stateName)
	    assertResult(eGen0)(instanceRef.stateData match {
	      case gd: StateData[_] => gd.generation
	      case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	  }
	  
	  "Continues gathering when report completed message received" in new Instance{
	    val instanceObj = instanceRef.underlyingActor
	    val eGen0 = mock[EvolvingGeneration[DullParams]]
//	    val gen0 = mock[Generation[DullParams]]
	    val stateData = StateData(eGen0, null, None)
	    
	    val report = mock[Report[DullParams]]
	    when(report.generationId).thenReturn(1)
	    val reportCompleted = ReportCompleted(report)
	    
	    instanceRef.setState(Gathering, stateData)
	    
	    // Action
	    instanceRef tell(reportCompleted, null)
	    
	    // Assertions
	    assertResult(Gathering)(instanceRef.stateName)
	    assertResult(eGen0)(instanceRef.stateData match {
	      case gd: StateData[_] => gd.generation
	      case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	  }
	}
	
	"When Flushing a completed generation" - {
	  "Discards incoming particless" in new Instance{
	    val instanceObj = instanceRef.underlyingActor
	    
	    val stateData = StateData(mock[EvolvingGeneration[DullParams]], null, None)
	    
	    val scored = mock[ScoredParticles[DullParams]]
	    
	    instanceRef.setState(Flushing, stateData)
	    
	    // Action
	    instanceRef tell(scored, null)
	    
	    // Assertion
	    assertResult(Flushing)(instanceRef.stateName)
	    assertResult(stateData)(instanceRef.stateData match {
	      case sd: StateData[_] => sd
	      case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	  }
	  
	  "Ignores any request to mix now" in new Instance{
	    val instanceObj = instanceRef.underlyingActor
	    
	    val stateData = StateData(mock[EvolvingGeneration[DullParams]], null, None)
	    
	    instanceRef.setState(Flushing, stateData)
	    
	    val mixNow = instanceObj.MixNow
	    
	    // Action
	    instanceRef tell(mixNow, null)
	    
	    // Assertion
	    assertResult(Flushing)(instanceRef.stateName)
	    assertResult(stateData)(instanceRef.stateData match {
	      case sd: StateData[_] => sd
	      case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	  }
	  
	  "and receives FlushComplete message" - {
	    val eGen0 = mock[EvolvingGeneration[DullParams]]
	    val flushedGeneration = mock[Generation[DullParams]]
	    
	    val flushingData = FlushingData(null, None)
	    
	    "shuts down if the required number of generations completed" in new Instance{
	      val workerProbe = TestProbe()
	      val reportingProbe = TestProbe()
	      val routerProbe = TestProbe()
	      
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.reportingActor).thenReturn(reportingProbe.ref)
	      when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	   
	      when(flushedGeneration.iteration).thenReturn(fiveGenerations)
 	      when(eGen0.previousGen).thenReturn(flushedGeneration)

	      
	      val algorithm = instanceObj.algorithm
	      
	      //TODO remove if not used
//	      val report = mock[Report[DullParams]]
//	      when(instanceObj.reporter.build(flushedGeneration)).thenReturn(report)
	      
	      val flushComplete = instanceObj.FlushComplete(eGen0)
	      
	      instanceRef.setState(Flushing, flushingData)
	      
	      // Action
	      instanceRef tell(flushComplete, null)
	      
	      // Assertion
	      routerProbe.expectMsg(Abort)
	      
	      assertResult(WaitingForShutdown)(instanceRef.stateName)
	      assertResult(flushedGeneration)(instanceRef.stateData match {
	        case gd: StateData[_] => gd.generation
	        case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	    }
	    
	    "starts generating more particles if more generations are required" in new Instance{
	      val workerProbe = TestProbe()
	      val reportingProbe = TestProbe()
	      val routerProbe = TestProbe()
	      
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.reportingActor).thenReturn(reportingProbe.ref)
	      when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	   
	      val prevWeights = Map[DullParams, Double]()
	      
	      when(flushedGeneration.iteration).thenReturn(1)
	      when(flushedGeneration.particleWeights).thenReturn(prevWeights)
	      when(eGen0.previousGen).thenReturn(flushedGeneration)
	      
	      val algorithm = instanceObj.algorithm
	      
	      //TODO needed?
//	      val report = mock[Report[DullParams]]
//	      when(algorithm.buildReport(flushedGeneration, instanceObj.config)).thenReturn(report)
	      
	      val flushComplete = instanceObj.FlushComplete(eGen0)
	      
	      instanceRef.setState(Flushing, flushingData)
	      
	      // Action
	      instanceRef tell(flushComplete, null)
	      
	      // Assertion
	      routerProbe.expectMsg(Broadcast(GenerateParticles(prevWeights, instanceObj.config)))
	      
	      assertResult(Gathering)(instanceRef.stateName)
	      assertResult(flushedGeneration)(instanceRef.stateData match {
	        case gd: StateData[_] => gd.generation
	        case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	    }
	  }
	  
	  "Stay flushing when report completed message received" in new Instance{
	    val instanceObj = instanceRef.underlyingActor
	    
	    val stateData = StateData(mock[EvolvingGeneration[DullParams]], null, None)
	    
	    instanceRef.setState(Flushing, stateData)
	    
	    val report = mock[Report[DullParams]]
	    val rc = ReportCompleted(report)
	    
	    // Action
	    instanceRef tell(rc, null)
	    
	    // Assertion
	    assertResult(Flushing)(instanceRef.stateName)
	    assertResult(stateData)(instanceRef.stateData match {
	      case sd: StateData[_] => sd
	      case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	  }
	}
	
	"When Waiting For Shutdown" - {
	  
	  "Reports to Client when waiting for shutdown" in new Instance{
	    val clientProbe = TestProbe()
	    val workerProbe = TestProbe()
	    
	    val instanceObj = instanceRef.underlyingActor

	    val eGen = mock[EvolvingGeneration[DullParams]]
	    val report = mock[Report[DullParams]]
	    val reportCompleted = ReportCompleted(report)
	    
	    val stateData = StateData(eGen, clientProbe.ref, None)
	    
	    instanceRef.setState(WaitingForShutdown, stateData)
	    
	    // Action
	    instanceRef tell(reportCompleted, workerProbe.ref)
	    
	    // Assertions
	    clientProbe.expectMsg(report)
	    
	    assertResult(WaitingForShutdown)(instanceRef.stateName)
	    assertResult(eGen)(instanceRef.stateData match {
	      case gd: StateData[_] => gd.generation
	      case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	  }
	}
}