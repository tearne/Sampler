package sampler.abc.actor.root.state

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, FreeSpecLike}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import sampler.abc._
import sampler.abc.actor.children.{NewScored, StatusReport}
import sampler.abc.actor.message.{MixPayload, ScoredParticles, WeighedParticles}
import sampler.abc.actor.root.state.task.RunningTask
import sampler.abc.actor.root.state.task.egen.{EGenUtil, EvolvingGeneration}
import sampler.abc.actor.root.{ChildActors, ChildRefs}

import scala.collection.immutable.Queue

class StateUtilTest extends TestKit(ActorSystem("MySpec")) with FreeSpecLike with MockitoSugar with BeforeAndAfterAll {

  val FromRemote = true
  type P = Int

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "StateUtil should" - {
    "allocate" - {
      "weighing job if available" in fail("TODO")
      "generating job if no weighing jobs available" in fail("TODO")
    }

    "process mix messages" - {
      "builds new 'running task'" in new SetupMocks {
        assertResult(newTask){
          instance.addScoredFromMixing(mixPayload, previousTask, null, childRefs)
        }
      }
      "sends a status report" in new SetupMocks {
        val mixMsgSender = TestProbe().ref
        instance.addScoredFromMixing(mixPayload, previousTask, mixMsgSender, childRefs)

        expectMsg(StatusReport(
          NewScored(100, mixMsgSender, FromRemote),
          newEGen,
          config
        ))
      }
    }

    "Have more tests" in fail("TODO")
  }

  trait SetupMocks {
    val childRefs = mock[ChildRefs]
    when(childRefs.reporter).thenReturn(testActor)

    // Build instance
    val eGenUtil = mock[EGenUtil]
    val config = mock[ABCConfig]
    val instance = new StateUtil(eGenUtil, config)

    // Build inputs
    val newScoredParticles = ScoredParticles((1 to 100).map(_ => mock[Scored[P]]))
    val mixPayload = MixPayload(newScoredParticles)
    val oldEGen = mock[EvolvingGeneration[P]]; val newEGen = mock[EvolvingGeneration[P]]
    val previousTask = mock[RunningTask[P]]
    when(previousTask.evolvingGeneration).thenReturn(oldEGen)
    when(eGenUtil.filterAndQueueUnweighedParticles(newScoredParticles, oldEGen)).thenReturn(newEGen)

    val newTask = RunningTask[P](
      config = null,
      client = null,
      evolvingGeneration = newEGen
    )
    when(previousTask.updateEvolvingGeneration(newEGen)).thenReturn(newTask)
  }
}
