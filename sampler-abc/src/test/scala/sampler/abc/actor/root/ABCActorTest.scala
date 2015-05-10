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
import scala.collection.immutable.Queue
import org.scalatest.BeforeAndAfter
import akka.actor.ActorRef

@RunWith(classOf[JUnitRunner])
class ABCActorTest 
		extends TestKit(ActorSystem("ABC"))
		with FreeSpecLike
		with BeforeAndAfter
		with BeforeAndAfterAll
		with MockitoSugar {
	
	val oneSecond = 1000l
	val hundredParticles = 100
	val fiveGenerations = 5
	val isFinal = true
	val terminateAtTargetGen = true	//TODO why would we have false?  Hasn't been tested
	
	//val model = mock[Model[DullParams]]
	
	//val gen1 = Generation(null, 1, Map[DullParams, Double](), 50)
	//val gen2 = Generation(null, 2, Map[DullParams, Double](), 20)

	//TODO before each
	var gen1: Generation[DullParams] = _
	var eGen1: EvolvingGeneration[DullParams] = _
	
	before {
		gen1 = Generation(null, 0, mock[Map[DullParams, Double]], 99)
		
		eGen1 = EvolvingGeneration(
			99, 
			gen1, 
			mock[ScoredParticles[DullParams]],//ScoredParticles(Seq.empty[Tagged[Scored[DullParams]]]), 
			mock[WeighedParticles[DullParams]], 
			mock[Queue[Long]])
	}
	
	
//	val eGen0 = {
//		val dueWeighing0 = ScoredParticles(Seq.empty[Tagged[Scored[DullParams]]])
//		  val tolerance0 = 30;
//	    val eGen0 = EvolvingGeneration(tolerance0, prevGen0, dueWeighing0, null, null)
//	}
	
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
	
	"When Idle / " - {
	  "Start msg sets internal state & sends Broadcast to generate particles" in new Instance {
			val routerProbe = TestProbe()
				  
			val instanceObj = instanceRef.underlyingActor
			when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
			
			val particleWeights = Map[DullParams, Double]()
	    
			// Action
			instanceRef ! Start(gen1)
		
			// Assertions
			routerProbe.expectMsg(Broadcast(GenerateParticles(gen1.particleWeights, instanceObj.config)))
			assertResult(Gathering)(instanceRef.stateName)
			assertResult(gen1)(instanceRef.stateData match {
				case gd: StateData[_] => gd.generation.previousGen
				case d => fail("Unexpected StateData type: "+d.getClass())
			})
	  }
	}
	
	"When Gathering / " - {
	  "and Failed msg arrives / " - {
	    //TODO delete
	  	val prevWeights = Map[DullParams, Double]()
	    val prevGen = Generation(null, 0, prevWeights, 0)
	    
	    "if zero weighing jobs then tell worker to start generating" in new Instance{
	    	val workerProbe = TestProbe()  
		  	val instanceObj = instanceRef.underlyingActor
		
		  	when(eGen1.dueWeighing.size).thenReturn(0)
		  	
		  	instanceRef.setState(Gathering, StateData(eGen1, null, None))
		  
		  	// Action
		  	instanceRef tell(Failed, workerProbe.ref)
		  
		  	// Assertions
		  	workerProbe.expectMsg(GenerateParticles(eGen1.previousGen.particleWeights, instanceObj.config))
		  	assertResult(Gathering)(instanceRef.stateName)
		  	assertResult(eGen1)(instanceRef.stateData match {
			  	case sd: StateData[_] => sd.generation
			  	case e => fail("Unexpected StateData type: "+e.getClass())
			  })
		  }
	  
	    "if weighing job available tell worker to weigh" in new Instance{
	      val workerProbe = TestProbe()
	      val instanceObj = instanceRef.underlyingActor

	      when(eGen1.dueWeighing.size).thenReturn(10)
	      instanceRef.setState(Gathering, StateData(eGen1, null, None))
	    	
	      val eGen2 = mock[EvolvingGeneration[DullParams]]
	      when(instanceObj.algorithm.emptyWeighingBuffer(eGen1)).thenReturn(eGen2)
	    	
	      // Action
	      instanceRef tell(Failed, workerProbe.ref)

	      // Assertion
	      workerProbe.expectMsg(
	      		WeighJob(
	      				eGen1.dueWeighing, 
	      				eGen1.previousGen.particleWeights, 
	      				eGen1.currentTolerance))

	      assertResult(Gathering)(instanceRef.stateName)
	      assertResult(eGen2)(instanceRef.stateData match {
	        case gd: StateData[_] => gd.generation
	        case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	    }
	  }
	  
	  "after worker sends new scored particles it gets a weighing job" in new Instance{
	    val clientProbe = TestProbe() //TODO would be better to offload this somehow
	    val workerProbe = TestProbe()
	  
		  val instanceObj = instanceRef.underlyingActor
				  
		  val eGen0 = mock[EvolvingGeneration[DullParams]]
		  instanceRef.setState(Gathering, StateData(eGen0, clientProbe.ref, None))
				  
		  val incomingScoredParticles = ScoredParticles(Seq.empty[Tagged[Scored[DullParams]]])
	    
		  when(instanceObj.algorithm.filterAndQueueUnweighedParticles(
				incomingScoredParticles,
				eGen0
		  )).thenReturn(eGen1)
						  
		  // Action
		  instanceRef tell(incomingScoredParticles, workerProbe.ref)
		  
		  // Assertions
		  workerProbe.expectMsg(
		  		WeighJob(
		  				eGen1.dueWeighing, 
		  				eGen1.previousGen.particleWeights, 
		  				eGen1.currentTolerance))
		  
		  assertResult(Gathering)(instanceRef.stateName)
		  val stateData = instanceRef.stateData.asInstanceOf[StateData[DullParams]]
	    assertResult(eGen1.emptyWeighingBuffer)(stateData.generation)
		  assertResult(clientProbe.ref)(stateData.client)
	  }
	  
	  "filters and queue particles from a MixPayload" in new Instance{
	    val clientProbe = TestProbe()
	    val routerProbe = TestProbe()
	    //val workerProbe = TestProbe()
	    
			val instanceObj = instanceRef.underlyingActor
			when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	    
			val eGen0 = mock[EvolvingGeneration[DullParams]]
	    
	    val payload = mock[MixPayload[DullParams]]
	    val scored = mock[ScoredParticles[DullParams]]
	    val seq = mock[Seq[Tagged[Scored[DullParams]]]]
	    when(scored.seq).thenReturn(seq)
	    when(payload.scoredParticles).thenReturn(scored)
	    
	    val algorithm = instanceObj.algorithm
	    when(algorithm.filterAndQueueUnweighedParticles(scored, eGen0)).thenReturn(eGen1)
		
	    instanceRef.setState(Gathering, StateData(eGen0, clientProbe.ref, None))
	    
	    // Action
	    instanceRef ! payload
	    
	    // Assert
	    assertResult(Gathering)(instanceRef.stateName)
	    val stateData = instanceRef.stateData.asInstanceOf[StateData[DullParams]]
	    assertResult(eGen1)(stateData match {
	    	case sd: StateData[_] => sd.generation
	    	case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	    assertResult(clientProbe.ref)(stateData.client)
	  }
	  
	  "and weighed particles arrive / " - {
	    val newlyWeighted = mock[WeighedParticles[DullParams]]
	    
	    val eGen0 = mock[EvolvingGeneration[DullParams]]
	    val eGen2 = mock[EvolvingGeneration[DullParams]]
	    
	    "do more weighing if more jobs already arrived and gen is incomplete" in new Instance {
	    	val clientRef = mock[ActorRef]
	    	val workerProbe = TestProbe()
	    	
	      val stateData0 = StateData(eGen0, clientRef, None)
	      instanceRef.setState(Gathering, stateData0)
	      
	      when(eGen1.dueWeighing.size).thenReturn(100)
	    	
	    	val algorithm = instanceRef.underlyingActor.algorithm
	    	when(algorithm.addWeightedParticles(newlyWeighted, eGen0)).thenReturn(eGen1)
	    	when(algorithm.isEnoughParticles(eGen1, config)).thenReturn(false)
	      when(algorithm.emptyWeighingBuffer(eGen1)).thenReturn(eGen2)
	      
	      // Action
	      instanceRef tell(newlyWeighted, workerProbe.ref)
	      
	      // Assertions
	    	workerProbe.expectMsg(
	    			WeighJob(
	    					eGen1.dueWeighing, 
	    					eGen1.previousGen.particleWeights, 
	    					eGen1.currentTolerance))
	      
		    val stateData = instanceRef.stateData.asInstanceOf[StateData[DullParams]]
		    assertResult(eGen2)(stateData match {
		    	case sd: StateData[_] => sd.generation
		    	case d => fail("Unexpected StateData type: "+d.getClass())
		    })
		    assertResult(clientRef)(stateData.client)
	    }
	    
	    "make more particles if gen incomplete and no weighing jobs waiting" in new Instance{
	      val clientRef = mock[ActorRef]
	    	val workerProbe = TestProbe()
	   
	      val algorithm = instanceRef.underlyingActor.algorithm
	      when(algorithm.addWeightedParticles(newlyWeighted, eGen0)).thenReturn(eGen1)
	      when(algorithm.isEnoughParticles(eGen1, config)).thenReturn(false)
	      
	      when(eGen1.dueWeighing.size).thenReturn(0)
	      
	      instanceRef.setState(
	      		Gathering, 
	      		StateData(eGen0, clientRef, None))
	      
	      // Action
	      instanceRef tell(newlyWeighted, workerProbe.ref)
	      
	      // Assertion
	      workerProbe.expectMsg(GenerateParticles(eGen1.previousGen.particleWeights, config))
	      
	      assertResult(Gathering)(instanceRef.stateName)
	      assertResult(eGen1)(instanceRef.stateData match {
		    	case gd: StateData[_] => gd.generation
		    	case d => fail("Unexpected StateData type: "+d.getClass())
		    })
		    assertResult(clientRef)(instanceRef.stateData.asInstanceOf[StateData[DullParams]].client)
	    }
	    
	    "start new generation if got enough particles but need more generations" in new Instance{
	      val clientRef = mock[ActorRef]
	    	val routerProbe = TestProbe()
	      val workerProbe = TestProbe()
	      val reportingProbe = TestProbe()
	      
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	      when(instanceObj.childActors.reportingActor).thenReturn(reportingProbe.ref)
	      
	      val algorithm = instanceObj.algorithm
	      when(algorithm.addWeightedParticles(newlyWeighted, eGen0)).thenReturn(eGen1)
	      when(algorithm.isEnoughParticles(eGen1, config)).thenReturn(true)
	      
	      
	      when(algorithm.flushGeneration(eGen1)).thenReturn(eGen2)
	      
	      val report = mock[Report[DullParams]]	
	      val completedGeneration = mock[Generation[DullParams]]
	    	val flushedWeightsTable = Map[DullParams, Double](DullParams() -> 0.5, DullParams() -> 0.5)
	      when(eGen2.previousGen).thenReturn(completedGeneration)
	      when(completedGeneration.particleWeights).thenReturn(flushedWeightsTable)
	      when(instanceObj.reporter.build(completedGeneration)).thenReturn(report)
	      
	      instanceRef.setState(Gathering, StateData(eGen0, clientRef, None))
	      
	      // Action
	      instanceRef tell(newlyWeighted, workerProbe.ref)
	      
	      // Expectations
	      routerProbe.expectMsg(Broadcast(Abort))
	      routerProbe.expectMsg(Broadcast(GenerateParticles(flushedWeightsTable, instanceObj.config)))
	      
	      reportingProbe.expectMsg(report)
	      
	      assertResult(Gathering)(instanceRef.stateName)
	      assertResult(eGen2)(instanceRef.stateData match {
	    		case sd: StateData[_] => sd.generation
	    		case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	      assertResult(clientRef)(instanceRef.stateData.asInstanceOf[StateData[DullParams]].client)
	    }
	    
	    "if got enough particles and generations then stop generating and await shutdown" in new Instance{
	      //TODO shouldn't it enter flushing state?!
	    	
	    	val clientRef = mock[ActorRef]
	    	val routerProbe = TestProbe()
	      val workerProbe = TestProbe()
	      val reportingProbe = TestProbe()
	      
	      val stateData0 = StateData(eGen1, clientRef, None)
	   
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	      when(instanceObj.childActors.reportingActor).thenReturn(reportingProbe.ref)
	      
	      val algorithm = instanceObj.algorithm
	      when(algorithm.addWeightedParticles(newlyWeighted, eGen1)).thenReturn(eGen2)	      
	      when(algorithm.isEnoughParticles(eGen2, instanceObj.config)).thenReturn(true)
	      
	      val completedGen = mock[Generation[DullParams]]

	      //TODO was needed for logging to avoid errors
//	      when(completedGen.iteration).thenReturn(fiveGenerations)	// Matches config in setup above
//	      when(completedGen.tolerance).thenReturn(hundredParticles)// Matches config in setup above
	      when(eGen2.previousGen).thenReturn(completedGen)
	      
	      val eGen3 = mock[EvolvingGeneration[DullParams]]
	      when(eGen3.currentIteration).thenReturn(fiveGenerations)// Matches config in setup above
	      when(algorithm.flushGeneration(eGen2)).thenReturn(eGen3)
	      
	      val report = mock[Report[DullParams]]	
	      val completedGeneration = mock[Generation[DullParams]]
	      val flushedWeightsTable = Map[DullParams, Double](DullParams() -> 0.5, DullParams() -> 0.5)
	      when(eGen3.previousGen).thenReturn(completedGeneration)
	      when(completedGeneration.particleWeights).thenReturn(flushedWeightsTable)
	      when(instanceObj.reporter.build(completedGeneration)).thenReturn(report)
	      
	      instanceRef.setState(Gathering, stateData0)
	      
	      // Action
	      instanceRef tell(newlyWeighted, workerProbe.ref)
	      
	      // Expectations
	      routerProbe.expectMsg(Broadcast(Abort))
	      reportingProbe.expectMsg(report)
	      
	      assertResult(WaitingForShutdown)(instanceRef.stateName)
	      assertResult(eGen3)(instanceRef.stateData match {
	    		case sd: StateData[_] => sd.generation
	    		case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	      assertResult(clientRef)(instanceRef.stateData.asInstanceOf[StateData[DullParams]].client)
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
	
	"When Flushing a completed generation / " - {
	  "incoming particles are discarded" in new Instance{
	    val instanceObj = instanceRef.underlyingActor
	    
	    val stateData = StateData(eGen1, null, None)
	    
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
	  
	  "mix timers are ignored" in new Instance{
	    val instanceObj = instanceRef.underlyingActor
	    
	    val stateData = StateData(eGen1, null, None)
	    
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
	  
	  "and receives FlushComplete message / " - {
	    val eGen0 = mock[EvolvingGeneration[DullParams]]
	    val flushedGeneration = mock[Generation[DullParams]]
	    
	    val flushingData = FlushingData(null, None)
	    
	    "shuts down if the required number of generations completed" in new Instance{
	      val workerProbe = TestProbe()
	      val reportingProbe = TestProbe()
	      val routerProbe = TestProbe()
	      
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.reportingActor).thenReturn(reportingProbe.ref)
//	      when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	   
	      when(flushedGeneration.iteration).thenReturn(fiveGenerations)
 	      when(eGen0.currentIteration).thenReturn(fiveGenerations)

	      
	      val algorithm = instanceObj.algorithm
	      
	      //TODO remove if not used
	      val report = mock[Report[DullParams]]
	      when(instanceObj.reporter.build(flushedGeneration)).thenReturn(report)
	      
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