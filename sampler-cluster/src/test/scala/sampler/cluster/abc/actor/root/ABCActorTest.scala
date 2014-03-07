package sampler.cluster.abc.actor.root

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
import sampler.cluster.abc.Model
import sampler.cluster.abc.Scored
import sampler.cluster.abc.actor.Failed
import sampler.cluster.abc.actor.GenerateJob
import sampler.cluster.abc.actor.Report
import sampler.cluster.abc.actor.ScoredParticles
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.actor.WeighJob
import sampler.cluster.abc.algorithm.Algorithm
import sampler.cluster.abc.algorithm.AlgorithmComponent
import sampler.cluster.abc.algorithm.Generation
import sampler.cluster.abc.config.ABCConfig
import sampler.cluster.abc.config.ClusterParameters
import sampler.cluster.abc.config.JobParameters
import sampler.cluster.abc.actor.MixPayload
import sampler.cluster.abc.actor.WeighedParticles
import sampler.cluster.abc.actor.Abort
import sampler.cluster.abc.actor.GenerateJob
import sampler.cluster.abc.actor.ReportCompleted
import sampler.cluster.abc.actor.ReportCompleted

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
	
	"When Idle" - {

	  "Initialise and generate a job" in {
	    val routerProbe = TestProbe()
	    val clientProbe = TestProbe()
				  
	    val instanceRef = getInstance
		val instanceObj = instanceRef.underlyingActor
		when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
		
		val prevWeights = Map[DullParams, Double]()
	    
	    val gen0 = mock[Generation[DullParams]]
	    when(gen0.prevWeightsTable).thenReturn(prevWeights)
	    
		// Action
		instanceRef tell(Start(gen0), clientProbe.ref)
		
		// Assertions
		routerProbe.expectMsg(Broadcast(GenerateJob(prevWeights, instanceObj.config)))
		assertResult(Gathering)(instanceRef.stateName)
		assertResult(gen0)(instanceRef.stateData match {
			case gd: StateData[_] => gd.generation
			case d => fail("Unexpected StateData type: "+d.getClass())
		})
	  }
	}
	
	"When Gathering" - {
	  "Failed event" - {
	    
	    val failed = Failed
	    
	    val prevWeights = Map[DullParams, Double]()
	    
	    val gen0 = mock[Generation[DullParams]]
	    when(gen0.prevWeightsTable).thenReturn(prevWeights)
	    
	    "Nothing due weighing generates a new job" in {
	      val workerProbe = TestProbe()
		  
	      val instanceRef = getInstance
		  val instanceObj = instanceRef.underlyingActor
		  
		  val dueWeighing = mock[ScoredParticles[DullParams]]
		  when(dueWeighing.size).thenReturn(0)
		  
		  when(gen0.dueWeighing).thenReturn(dueWeighing)
			
		  instanceRef.setState(Gathering, StateData(gen0, null, None))
		  
		  // Action
		  instanceRef tell(failed, workerProbe.ref)
		  
		  // Assertion
		  workerProbe.expectMsg(GenerateJob(prevWeights, instanceObj.config))
		  
		  assertResult(Gathering)(instanceRef.stateName)
		  assertResult(gen0)(instanceRef.stateData match {
		  	case gd: StateData[_] => gd.generation
		  	case d => fail("Unexpected StateData type: "+d.getClass())
		  })
	  }
	  
	    "With particles due weighing instructs to weigh" in {
	      val workerProbe = TestProbe()

	      val instanceRef = getInstance
	      val instanceObj = instanceRef.underlyingActor

	      val dueWeighing = mock[ScoredParticles[DullParams]]
	      when(dueWeighing.size).thenReturn(10)
	    	
	      when(gen0.dueWeighing).thenReturn(dueWeighing)
	    	
	      val gen1 = mock[Generation[DullParams]]

	      instanceRef.setState(Gathering, StateData(gen0, null, None))
	    	
	      val algorithm = instanceObj.algorithm
	      when(algorithm.emptyWeighingBuffer(gen0)).thenReturn(gen1)
	    	
	      // Action
	      instanceRef tell(failed, workerProbe.ref)

	      // Assertion
	      workerProbe.expectMsg(WeighJob(dueWeighing, prevWeights, 0))

	      assertResult(Gathering)(instanceRef.stateName)
	      assertResult(gen1)(instanceRef.stateData match {
	        case gd: StateData[_] => gd.generation
	        case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	    }
	  }
	  
	  "Receives scored particles and weights them" in {
	      val clientProbe = TestProbe()
	      val workerProbe = TestProbe()
	  
		  val instanceRef = getInstance
		  val instanceObj = instanceRef.underlyingActor
				  
		  val gen0 = Generation(null, null, null, null, 0, 0, Map[DullParams, Double]())
				  
		  instanceRef.setState(Gathering, StateData(gen0, clientProbe.ref, None))
				  
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
	  }
	  
	  "Filters and queue particles from a MixPayload" in {
	    val clientProbe = TestProbe()
	    val routerProbe = TestProbe()
	    val workerProbe = TestProbe()
	    
	    val instanceRef = getInstance
		val instanceObj = instanceRef.underlyingActor
		when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	    
		val gen0 = mock[Generation[DullParams]]
		val gen1 = mock[Generation[DullParams]]
	    
	    when(gen1.dueWeighing).thenReturn(ScoredParticles(Seq.empty[Tagged[Scored[DullParams]]]))
	    
	    val payload = mock[MixPayload[DullParams]]
	    val scored = mock[ScoredParticles[DullParams]]
	    when(scored.seq).thenReturn(Seq())
	    when(payload.tss).thenReturn(scored)
	    
	    val algorithm = instanceObj.algorithm
	    when(algorithm.filterAndQueueForWeighing(scored, gen0)).thenReturn(gen1)
		
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
	  
	  "Adding weighed particles" - {
	    
	    val weighted = mock[WeighedParticles[DullParams]]
	    
	    val gen0 = mock[Generation[DullParams]]
	    val gen1 = mock[Generation[DullParams]]
	    
	    val prevWeights = Map[DullParams, Double]()
	    when(gen1.prevWeightsTable).thenReturn(prevWeights)
	    
	    "Then generate job to gather more particles" in {
	      val workerProbe = TestProbe()
	      val stateData0 = StateData(gen0, null, None)
	   
	      val instanceRef = getInstance
	      val instanceObj = instanceRef.underlyingActor
	      
	      val algorithm = instanceObj.algorithm
	      when(algorithm.addWeighted(weighted, gen0)).thenReturn(gen1)
	      
	      when(algorithm.isEnoughParticles(gen1, instanceObj.config)).thenReturn(false)
	      
	      val dueWeighing = mock[ScoredParticles[DullParams]]
	      when(dueWeighing.size).thenReturn(0)
	      
	      when(gen1.dueWeighing).thenReturn(dueWeighing)
	      
	      instanceRef.setState(Gathering, stateData0)
	      
	      // Action
	      instanceRef tell(weighted, workerProbe.ref)
	      
	      // Assertion
	      workerProbe.expectMsg(GenerateJob(prevWeights, instanceObj.config))
	      
	      assertResult(Gathering)(instanceRef.stateName)
	      assertResult(gen1)(instanceRef.stateData match {
	    	case gd: StateData[_] => gd.generation
	    	case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	    }
	    
	    "Start new generation if gathered enough particles but not met target for termination" in {
	      val routerProbe = TestProbe()
	      val workerProbe = TestProbe()
	      val reportingProbe = TestProbe()
	      
	      val stateData0 = StateData(gen0, null, None)
	   
	      val instanceRef = getInstance
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	      when(instanceObj.childActors.reportingActor).thenReturn(reportingProbe.ref)
	      
	      val algorithm = instanceObj.algorithm
	      when(algorithm.addWeighted(weighted, gen0)).thenReturn(gen1)
	      
	      when(algorithm.isEnoughParticles(gen1, instanceObj.config)).thenReturn(true)
	      
	      val flushedWeightsTable = Map[DullParams, Double](DullParams() -> 0.5, DullParams() -> 0.5)
	      
	      val flushedGen = mock[Generation[DullParams]]
	      when(flushedGen.currentIteration).thenReturn(1)
	      when(flushedGen.currentTolerance).thenReturn(100)
	      when(flushedGen.prevWeightsTable).thenReturn(flushedWeightsTable)
	      
	      when(algorithm.flushGeneration(gen1, instanceObj.config.job.numParticles, instanceObj.config.cluster.particleMemoryGenerations))
	      	.thenReturn(flushedGen)
	      
	      val report = mock[Report[DullParams]]	
	      	
	      when(algorithm.buildReport(flushedGen, instanceObj.config)).thenReturn(report)
	      
	      instanceRef.setState(Gathering, stateData0)
	      
	      // Action
	      instanceRef tell(weighted, workerProbe.ref)
	      
	      // Assertion
	      routerProbe.expectMsg(Broadcast(Abort))
	      
	      routerProbe.expectMsg(Broadcast(GenerateJob(flushedWeightsTable, instanceObj.config)))
	      
	      assertResult(Gathering)(instanceRef.stateName)
	      assertResult(flushedGen)(instanceRef.stateData match {
	    	case gd: StateData[_] => gd.generation
	    	case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	    }
	    
	    // Failing test
	    "Abort and wait for shutdown if gathered enough particles and completed target generations" in {
	      val routerProbe = TestProbe()
	      val workerProbe = TestProbe()
	      val reportingProbe = TestProbe()
	      
	      val stateData0 = StateData(gen0, null, None)
	   
	      val instanceRef = getInstance
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
	      when(instanceObj.childActors.reportingActor).thenReturn(reportingProbe.ref)
	      
	      val algorithm = instanceObj.algorithm
	      when(algorithm.addWeighted(weighted, gen0)).thenReturn(gen1)
	      
	      when(algorithm.isEnoughParticles(gen1, instanceObj.config)).thenReturn(true)
	      
	      val flushedGen = mock[Generation[DullParams]]
	      when(flushedGen.currentIteration).thenReturn(5)	// Matches config target
	      when(flushedGen.currentTolerance).thenReturn(100)
	      
	      when(algorithm.flushGeneration(gen1, instanceObj.config.job.numParticles, instanceObj.config.cluster.particleMemoryGenerations))
	      	.thenReturn(flushedGen)
	      
	      val report = mock[Report[DullParams]]	
	      	
	       when(algorithm.buildReport(flushedGen, instanceObj.config)).thenReturn(report)
	      	
	      instanceRef.setState(Gathering, stateData0)
	      
	      // Action
	      instanceRef tell(weighted, workerProbe.ref)
	      
	      // Assertion
	      routerProbe.expectMsg(Broadcast(Abort))
	      
	      assertResult(WaitingForShutdown)(instanceRef.stateName)
	      assertResult(flushedGen)(instanceRef.stateData match {
	    	case gd: StateData[_] => gd.generation
	    	case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	    }
	  }
	  
	  "Builds a new payload and sends message to mix it in when instructed to mix now" in {
	    val broadcasterProbe = TestProbe()
	    val workerProbe = TestProbe()
	    
	    val instanceRef = getInstance
	    val instanceObj = instanceRef.underlyingActor
	    when(instanceObj.childActors.broadcaster).thenReturn(broadcasterProbe.ref)
	    
	    val gen0 = mock[Generation[DullParams]]
	    
	    val stateData = StateData(gen0, null, None)
	    
	    val scored = ScoredParticles(Seq(Tagged(Scored(DullParams(), Seq(1.0)), 100)))
	    
	    val payload = Some(scored)
	    
	    val algorithm = instanceObj.algorithm
	    
	    when(algorithm.buildMixPayload(gen0, instanceObj.config)).thenReturn(payload)
	    
	    instanceRef.setState(Gathering, stateData)
	    
	    val mixNow = instanceObj.MixNow
	    
	    // Action
	    instanceRef tell(mixNow, workerProbe.ref)
	    
	    // Assertions
	    broadcasterProbe.expectMsg(MixPayload(scored))
	    
	    assertResult(Gathering)(instanceRef.stateName)
	    assertResult(gen0)(instanceRef.stateData match {
	      case gd: StateData[_] => gd.generation
	      case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	  }
	  
	  "Stays gathering when report completed message received" in {
	    val instanceRef = getInstance
	    val instanceObj = instanceRef.underlyingActor
	    
	    val gen0 = mock[Generation[DullParams]]
	    val stateData = StateData(gen0, null, None)
	    
	    val reportCompleted = mock[ReportCompleted[DullParams]]
	    
	    instanceRef.setState(Gathering, stateData)
	    
	    // Action
	    instanceRef tell(reportCompleted, null)
	    
	    // Assertions
	    
	    assertResult(Gathering)(instanceRef.stateName)
	    assertResult(gen0)(instanceRef.stateData match {
	      case gd: StateData[_] => gd.generation
	      case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	  }
	}
	
//	"When Flushing" - {
//	  "Ignores any new particles" in {
//	    
//	  }
//	  
//	  "Ignores any request to mix now" in {
//	    
//	  }
//	  
//	  "Receiving FlushComplete message" - {
//	    "Shuts down when required number of generations reached" in {
//	      
//	    }
//	    
//	    "Generates a job for more particle gathering when more generations required" in {
//	      
//	    }
//	  }
//	  
//	  "Stay flushing when report completed message received" in {
//	    
//	  }
//	}
	
	"When Waiting For Shutdown" - {
	  
	  "Reports to Client when waiting for shutdown" in {
	    val clientProbe = TestProbe()
	    val workerProbe = TestProbe()
	    
	    val instanceRef = getInstance
	    val instanceObj = instanceRef.underlyingActor

	    val gen = mock[Generation[DullParams]]
	    val report = mock[Report[DullParams]]
	    val reportCompleted = ReportCompleted(report)
	    
	    val stateData = StateData(gen, clientProbe.ref, null)
	    
	    instanceRef.setState(WaitingForShutdown, stateData)
	    
	    // Action
	    instanceRef tell(reportCompleted, workerProbe.ref)
	    
	    // Assertions
	    clientProbe.expectMsg(report)
	    
	    assertResult(WaitingForShutdown)(instanceRef.stateName)
	    assertResult(gen)(instanceRef.stateData match {
	      case gd: StateData[_] => gd.generation
	      case d => fail("Unexpected StateData type: "+d.getClass())
	    })
	  }
	}
}

