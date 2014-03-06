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
		
	  val routerProbe = TestProbe()
	  val clientProbe = TestProbe()

	  "Initialise and generate a job" in {
	    val instanceRef = getInstance
		val instanceObj = instanceRef.underlyingActor
		when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)
		
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
	  //TODO add test for reporting actions
	  
	  val clientProbe = TestProbe()
	  val routerProbe = TestProbe()
	  val workerProbe = TestProbe()
	  
	  "Failed event" - {
	    
	    val failed = Failed
	    
	    val prevWeights = Map[DullParams, Double]()
	    
	    val gen0 = mock[Generation[DullParams]]
	    when(gen0.prevWeightsTable).thenReturn(prevWeights)
	    
	    "Nothing due weighing generates a new job" in {
		  val instanceRef = getInstance
		  
		  val instanceObj = instanceRef.underlyingActor
		  when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)
		  
		  val dueWeighing = mock[ScoredParticles[DullParams]]
		  when(dueWeighing.size).thenReturn(0)
		  
		  when(gen0.dueWeighing).thenReturn(dueWeighing)
			
		  instanceRef.setState(Gathering, StateData(gen0, clientProbe.ref, None))
		  
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
	    	val instanceRef = getInstance
	    			
	    	val instanceObj = instanceRef.underlyingActor
	    	when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)

	    	val dueWeighing = mock[ScoredParticles[DullParams]]
	    	when(dueWeighing.size).thenReturn(10)
	    	
	    	when(gen0.dueWeighing).thenReturn(dueWeighing)
	    	
	    	val gen1 = mock[Generation[DullParams]]

	    	instanceRef.setState(Gathering, StateData(gen0, clientProbe.ref, None))
	    	
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
		  val instanceRef = getInstance
		  val instanceObj = instanceRef.underlyingActor
		  when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)
				  
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
	    val instanceRef = getInstance
		val instanceObj = instanceRef.underlyingActor
		when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)
	    
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
	    val stateData0 = StateData(gen0, clientProbe.ref, None)
	    
	    val prevWeights = Map[DullParams, Double]()
	    when(gen1.prevWeightsTable).thenReturn(prevWeights)
	    
	    "Then generate job to gather more particles" in {
	      val instanceRef = getInstance
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)
	      
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
	    
	    "Abort when enough particles gathered and go to flushing" in {
	      val instanceRef = getInstance
	      val instanceObj = instanceRef.underlyingActor
	      when(instanceObj.childActors.workerRouter).thenReturn(routerProbe.ref)
	      
	      val algorithm = instanceObj.algorithm
	      when(algorithm.addWeighted(weighted, gen0)).thenReturn(gen1)
	      
	      when(algorithm.isEnoughParticles(gen1, instanceObj.config)).thenReturn(true)
	      
	      instanceRef.setState(Gathering, stateData0)
	      
	      // Action
	      instanceRef tell(weighted, workerProbe.ref)
	      
	      // Assertion
	      routerProbe.expectMsg(Broadcast(Abort))
	      
	      assertResult(Flushing)(instanceRef.stateName)
	      assertResult(gen1)(instanceRef.stateData match {
	    	case gd: StateData[_] => gd.generation
	    	case d => fail("Unexpected StateData type: "+d.getClass())
	      })
	    }
	    
	  }
	}
	
}



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