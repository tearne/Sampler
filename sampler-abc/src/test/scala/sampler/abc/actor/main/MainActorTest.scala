package sampler.abc.actor.main

import org.mockito.Mockito.when
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
import sampler.abc.config.ABCConfig
import sampler.abc.config.ClusterParameters
import sampler.abc.config.JobParameters
import akka.actor.Cancellable
import sampler.abc.actor.main.component.helper.Getters
import scala.collection.immutable.Queue
import org.scalatest.BeforeAndAfter
import akka.actor.ActorRef
import sampler.abc.actor.sub.FlushComplete
import sampler.abc.Population
import sampler.abc.actor.main.component.WorkDispatcherComponent
import sampler.abc.actor.sub.GenerateParticlesFrom
import sampler.abc.actor.sub.Abort
import sampler.abc.actor.sub.WeighJob
import sampler.abc.actor.main.component.ChildActorsComponent
import sampler.abc.actor.main.component.HelperComponent
import sampler.abc.actor.sub.flushing.GenerationFlusher
import sampler.abc.actor.main.component.Helper

class MainActorTest
		extends TestKit(ActorSystem("ABC"))
		with FreeSpecLike
		with BeforeAndAfterAll
		with MockitoSugar {

	val noMixing = 0l
	val hundredParticles = 100
	val threeGenerations = 3
	val isFinal = true
	val terminateAtTargetGen = true //TODO false

	case class TestParams()

	class TestableMainActor(
		val model: Model[TestParams],
		val config: ABCConfig,
		val reportHandler: Option[Population[TestParams] => Unit],
		override val getters: Getters)
			extends MainActor[TestParams]
			with ChildActorsComponent[TestParams]
//			with WorkDispatcherComponent 
			with HelperComponent {
		val childActors = mock[ChildActors]
		val generationFlusher = mock[GenerationFlusher]
//		val workDispatcher = context.dispatcher
		val helper = mock[Helper]

//		val distributionBuilder = sampler.data.DistributionBuilder
		val random = sampler.math.Random
	}

	override def afterAll {
		TestKit.shutdownActorSystem(system)
	}

	trait Setup {
		val model = mock[Model[TestParams]]
		val config = ABCConfig(
			JobParameters(hundredParticles, 0, threeGenerations),
			null,
			ClusterParameters(terminateAtTargetGen, 0, 0l, 0, noMixing, 0l, 0l))
		val reportAction = None
		val getters = mock[Getters]
		when(getters.getMixRateMS(config)).thenReturn(noMixing)

		val instanceRef = TestFSMRef(new TestableMainActor(model, config, reportAction, getters))
		val instanceObj = instanceRef.underlyingActor
		when(instanceObj.childActors.reporter).thenReturn(TestProbe().ref)

		val clientRef = TestProbe().ref

		//TODO can just do mock[Population] now?
		var gen1: Population[TestParams] = Population( mock[Map[TestParams, Double]], 1, 99)
		var eGen1: EvolvingGeneration[TestParams] = EvolvingGeneration(
			99.9,
			gen1,
			mock[ScoredParticles[TestParams]],
			mock[WeighedParticles[TestParams]],
			mock[Queue[Long]])
	}

	"When Idle / " - {
		"Start msg sends Broadcast to generate particles" in new Setup {
			val routerProbe = TestProbe()

			when(instanceObj.childActors.router).thenReturn(routerProbe.ref)

			val particleWeights = Map[TestParams, Double]()

			// Action
			instanceRef ! Start(gen1)

			// Assertions
			routerProbe.expectMsg(Broadcast(GenerateParticlesFrom(gen1, instanceObj.config)))
			assertResult(Gathering)(instanceRef.stateName)
			assertResult(gen1)(instanceRef.stateData match {
				case gd: StateData[_] => gd.generation.previousGen
				case d => fail("Unexpected StateData type: " + d.getClass())
			})
		}
	}

	"When Gathering / " - {
		"and Failed msg arrives / " - {
			"if zero weighing jobs then tell worker to start generating again" in new Setup {
				val workerProbe = TestProbe()

				when(eGen1.dueWeighing.size).thenReturn(0)

				val state = StateData(eGen1, null, None)
				instanceRef.setState(Gathering, state)

				// Action
				instanceRef tell (Failed, workerProbe.ref)

				// Assertions
				workerProbe.expectMsg(GenerateParticlesFrom(
					eGen1.previousGen,
					instanceObj.config))
				assertResult(Gathering)(instanceRef.stateName)
				assertResult(state)(instanceRef.stateData match {
					case sd: StateData[_] => sd
					case e => fail("Unexpected StateData type: " + e.getClass())
				})
			}

			"if weighing job available tell worker to weigh" in new Setup {
				val workerProbe = TestProbe()

				when(eGen1.dueWeighing.size).thenReturn(10)
				instanceRef.setState(Gathering, StateData(eGen1, clientRef, None))

				val eGen2 = mock[EvolvingGeneration[TestParams]]
				when(instanceObj.helper.emptyWeighingBuffer(eGen1)).thenReturn(eGen2)

				// Action
				instanceRef tell (Failed, workerProbe.ref)

				// Assertion
				workerProbe.expectMsg(
					WeighJob(
						eGen1.dueWeighing,
						eGen1.previousGen,
						eGen1.currentTolerance))

				assertResult(Gathering)(instanceRef.stateName)
				assertResult(StateData(eGen2, clientRef, None))(instanceRef.stateData match {
					case sd: StateData[_] => sd
					case d => fail("Unexpected StateData type: " + d.getClass())
				})
			}
		}

		"when new scored particles arrive from worker it gets a weighing job back" in new Setup {
			val workerProbe = TestProbe()

			val eGen0 = mock[EvolvingGeneration[TestParams]]
			instanceRef.setState(Gathering, StateData(eGen0, clientRef, None))

			val incomingScoredParticles = ScoredParticles(Seq.empty[Tagged[Scored[TestParams]]])

			when(instanceObj.helper.filterAndQueueUnweighedParticles(
				incomingScoredParticles,
				eGen0)).thenReturn(eGen1)

			// Action
			instanceRef tell (incomingScoredParticles, workerProbe.ref)

			// Assertions
			workerProbe.expectMsg(
				WeighJob(
					eGen1.dueWeighing,
					eGen1.previousGen,
					eGen1.currentTolerance))

			assertResult(Gathering)(instanceRef.stateName)
			val expectedState = StateData(
				eGen1.emptyWeighingBuffer,
				clientRef,
				None)
			assertResult(expectedState)(instanceRef.stateData match {
				case sd: StateData[_] => sd
				case d => fail("Unexpected StateData type: " + d.getClass)
			})
		}

		"filters and queues particles from a MixPayload message" in new Setup {
			val routerProbe = TestProbe()

			when(instanceObj.childActors.router).thenReturn(routerProbe.ref)

			val eGen0 = mock[EvolvingGeneration[TestParams]]

			val payload = mock[MixPayload[TestParams]]
			val scored = mock[ScoredParticles[TestParams]]
			val seq = mock[Seq[Tagged[Scored[TestParams]]]]
			when(scored.seq).thenReturn(seq)
			when(payload.scoredParticles).thenReturn(scored)

			when(instanceObj.helper.filterAndQueueUnweighedParticles(scored, eGen0)).thenReturn(eGen1)

			instanceRef.setState(Gathering, StateData(eGen0, clientRef, None))

			// Action
			instanceRef ! payload

			// Assert
			assertResult(Gathering)(instanceRef.stateName)
			val expectedState = StateData(eGen1, clientRef, None)
			assertResult(expectedState)(instanceRef.stateData match {
				case sd: StateData[_] => sd
				case d => fail("Unexpected StateData type: " + d.getClass())
			})
		}

		"and weighed particles arrive / " - {
			val newlyWeighted = mock[WeighedParticles[TestParams]]

			val eGen0 = mock[EvolvingGeneration[TestParams]]
			val eGen2 = mock[EvolvingGeneration[TestParams]]

			"if generation in progress is incomplete / " - {
				"if weighing jobs pending then send to worker" in new Setup {
					val workerProbe = TestProbe()

					val stateData0 = StateData(eGen0, clientRef, None)
					instanceRef.setState(Gathering, stateData0)

					when(eGen1.dueWeighing.size).thenReturn(100)

					val helper = instanceRef.underlyingActor.helper
					when(helper.addWeightedParticles(newlyWeighted, eGen0)).thenReturn(eGen1)
					when(helper.isEnoughParticles(eGen1, config)).thenReturn(false)
					when(helper.emptyWeighingBuffer(eGen1)).thenReturn(eGen2)

					// Action
					instanceRef tell (newlyWeighted, workerProbe.ref)

					// Assertions
					workerProbe.expectMsg(
						WeighJob(
							eGen1.dueWeighing,
							eGen1.previousGen,
							eGen1.currentTolerance))

					val expectedState = StateData(eGen2, clientRef, None)
					assertResult(expectedState)(instanceRef.stateData match {
						case sd: StateData[_] => sd
						case d => fail("Unexpected StateData type: " + d.getClass())
					})
				}

				"instruct worker to make more particles if no weiging jobs pending" in new Setup {
					val workerProbe = TestProbe()

					val helper = instanceRef.underlyingActor.helper
					when(helper.addWeightedParticles(newlyWeighted, eGen0)).thenReturn(eGen1)
					when(helper.isEnoughParticles(eGen1, config)).thenReturn(false)

					when(eGen1.dueWeighing.size).thenReturn(0)

					instanceRef.setState(
						Gathering,
						StateData(eGen0, clientRef, None))

					// Action
					instanceRef tell (newlyWeighted, workerProbe.ref)

					// Assertion
					workerProbe.expectMsg(GenerateParticlesFrom(
						eGen1.previousGen,
						config))

					assertResult(Gathering)(instanceRef.stateName)
					val expectedState = StateData(eGen1, clientRef, None)
					assertResult(eGen1)(instanceRef.stateData match {
						case gd: StateData[_] => gd.generation
						case d => fail("Unexpected StateData type: " + d.getClass())
					})
				}
			}

			"if generation is complete then flush it" in new Setup {
				val routerProbe = TestProbe()
				val workerProbe = TestProbe()
				val flusherProbe = TestProbe()

				val stateData0 = StateData(eGen1, clientRef, None)

				when(instanceObj.childActors.router).thenReturn(routerProbe.ref)
				when(instanceObj.childActors.flusher).thenReturn(flusherProbe.ref)

				val helper = instanceObj.helper
				when(helper.addWeightedParticles(newlyWeighted, eGen1)).thenReturn(eGen2)
				when(helper.isEnoughParticles(eGen2, instanceObj.config)).thenReturn(true)

				instanceRef.setState(Gathering, stateData0)

				val inProgressGen = mock[Population[TestParams]]

				when(eGen2.previousGen).thenReturn(inProgressGen)

				// Action
				instanceRef tell (newlyWeighted, workerProbe.ref)

				// Expectations
				routerProbe.expectMsg(Broadcast(Abort))
				flusherProbe.expectMsg(eGen2)

				assertResult(Flushing)(instanceRef.stateName)
				val resultState = instanceRef.stateData match {
					case fd: FlushingData => fd
					case d => fail("Unexpected StateData type: " + d.getClass())
				}
				assertResult(stateData0.getFlushingData)(resultState)
				assertResult(clientRef)(resultState.client)
			}
		}

		"Acts on mix now instruction" in new Setup {
			val broadcasterProbe = TestProbe()
			val workerProbe = TestProbe()

			when(instanceObj.childActors.broadcaster).thenReturn(broadcasterProbe.ref)

			val eGen0 = mock[EvolvingGeneration[TestParams]]

			val scoredParticles = ScoredParticles(Seq(Tagged(Scored(TestParams(), Seq(1.0)), 100)))
			val mixPayload = Some(scoredParticles)

			when(instanceObj.helper.buildMixPayload(eGen0, instanceObj.config)).thenReturn(mixPayload)

			// TODO Factor out to common?
			val stateData = StateData(eGen0, null, None)
			instanceRef.setState(Gathering, stateData)

			// Action
			instanceRef tell (MixNow, workerProbe.ref)

			// Assertions
			broadcasterProbe.expectMsg(MixPayload(scoredParticles))

			assertResult(Gathering)(instanceRef.stateName)
			assertResult(eGen0)(instanceRef.stateData match {
				case gd: StateData[_] => gd.generation
				case d => fail("Unexpected StateData type: " + d.getClass())
			})
		}

		"Continues gathering when report completed message received" in new Setup {
			val eGen0 = mock[EvolvingGeneration[TestParams]]
			val stateData = StateData(eGen0, null, None)

			instanceRef.setState(Gathering, stateData)

			// Action
			instanceRef tell (ReportCompleted, null)

			// Assertions
			assertResult(Gathering)(instanceRef.stateName)
			assertResult(eGen0)(instanceRef.stateData match {
				case gd: StateData[_] => gd.generation
				case d => fail("Unexpected StateData type: " + d.getClass())
			})
		}
	}

	"When Flushing a completed generation / " - {
		
		"incoming weighed particles are discarded" in new Setup {
			val stateData = StateData(eGen1, clientRef, None)
			val weighed = mock[WeighedParticles[TestParams]]

			instanceRef.setState(Flushing, stateData)
			
			// Action
			instanceRef tell (weighed, null)
			
			// Assertion - no changes to state
			assertResult(Flushing)(instanceRef.stateName)
			assertResult(stateData)(instanceRef.stateData match {
				case sd: StateData[_] => sd
				case d => fail("Unexpected StateData type: " + d.getClass())
			})
		}
		
		"incoming scored particles are discarded" in new Setup {
			val stateData = StateData(eGen1, clientRef, None)

			val scored = mock[ScoredParticles[TestParams]]

			instanceRef.setState(Flushing, stateData)

			// Action
			instanceRef tell (scored, null)

			// Assertion - no changes to state
			assertResult(Flushing)(instanceRef.stateName)
			assertResult(stateData)(instanceRef.stateData match {
				case sd: StateData[_] => sd
				case d => fail("Unexpected StateData type: " + d.getClass())
			})
		}

		"mix now instructions are ignored" in new Setup {
			val stateData = StateData(eGen1, null, None)

			instanceRef.setState(Flushing, stateData)

			// Action
			instanceRef tell (MixNow, null)

			// Assertion
			assertResult(Flushing)(instanceRef.stateName)
			assertResult(stateData)(instanceRef.stateData match {
				case sd: StateData[_] => sd
				case d => fail("Unexpected StateData type: " + d.getClass())
			})
		}

		"and receives FlushComplete message / " - {
			val flushingStateData = FlushingData(null, None)

			"reports and shuts down if the required number of generations completed" in new Setup {
				val workerProbe = TestProbe()
				val reporterProbe = TestProbe()
				val routerProbe = TestProbe()

				when(instanceObj.childActors.reporter).thenReturn(reporterProbe.ref)
				when(instanceObj.childActors.router).thenReturn(routerProbe.ref)

				val eGen0 = mock[EvolvingGeneration[TestParams]]
				val flushedGen = mock[Population[TestParams]]
				when(flushedGen.iteration).thenReturn(threeGenerations)
				when(eGen0.previousGen).thenReturn(flushedGen)

				instanceRef.setState(Flushing, flushingStateData)

				// Action
				instanceRef ! FlushComplete(eGen0)

				// Assertion
				routerProbe.expectMsg(Abort)
				reporterProbe.expectMsg(flushedGen)

				assertResult(WaitingForShutdown)(instanceRef.stateName)
				assertResult(eGen0)(instanceRef.stateData match {
					case gd: StateData[_] => gd.generation
					case d => fail("Unexpected StateData type: " + d.getClass())
				})
			}

			"starts building next generation if required" in new Setup {
				val workerProbe = TestProbe()
				val reporterProbe = TestProbe()
				val routerProbe = TestProbe()

				when(instanceObj.childActors.reporter).thenReturn(reporterProbe.ref)
				when(instanceObj.childActors.router).thenReturn(routerProbe.ref)

				val prevWeights = Map[TestParams, Double]()

				instanceRef.setState(Flushing, flushingStateData)

				// Action
				instanceRef tell (FlushComplete(eGen1), null)

				// Assertion
				routerProbe.expectMsg(Broadcast(GenerateParticlesFrom(
					eGen1.previousGen,
					instanceObj.config)))

				assertResult(Gathering)(instanceRef.stateName)
				assertResult(eGen1)(instanceRef.stateData match {
					case sd: StateData[_] => sd.generation
					case d => fail("Unexpected StateData type: " + d.getClass())
				})
			}
		}

		"Stay flushing when report completed message received" in new Setup {
			val stateData = StateData(mock[EvolvingGeneration[TestParams]], null, None)

			instanceRef.setState(Flushing, stateData)

			// Action
			instanceRef tell (ReportCompleted, null)

			// Assertion
			assertResult(Flushing)(instanceRef.stateName)
			assertResult(stateData)(instanceRef.stateData match {
				case sd: StateData[_] => sd
				case d => fail("Unexpected StateData type: " + d.getClass())
			})
		}
	}

	"When Waiting For Shutdown /" - {
		//Duplication with tests above
		"Ignores MixNow messages" in fail("TODO") // TODO
		"incoming scored particles are discarded" in fail("TODO") // TODO
		"incoming weighed particles are discarded" in fail("TODO") // TODO
		
		"Reports to Client when waiting for shutdown" in new Setup {
			val clientProbe = TestProbe()
			val workerProbe = TestProbe()

			val eGen = mock[EvolvingGeneration[TestParams]]
			val flushedGen = mock[Population[TestParams]]
			when(eGen.previousGen).thenReturn(flushedGen)

			val stateData = StateData(eGen, clientProbe.ref, None)

			instanceRef.setState(WaitingForShutdown, stateData)

			// Action
			instanceRef tell (ReportCompleted, workerProbe.ref)

			// Assertions
			clientProbe.expectMsg(flushedGen)

			assertResult(WaitingForShutdown)(instanceRef.stateName)
			assertResult(eGen)(instanceRef.stateData match {
				case gd: StateData[_] => gd.generation
				case d => fail("Unexpected StateData type: " + d.getClass())
			})
		}
	}
}