package sampler.abc.actor.sub

import org.scalatest.FreeSpecLike
import akka.testkit.TestKit
import akka.actor.ActorSystem

//class WorkerActorTest extends TestKit(ActorSystem("ABC-test")) with FreeSpecLike{
//  "Worker actor should" - {
//  	"start in Idle state with uninitialised data" in fail("TODO")
//  	"when Idle" - {
//  		"start generating job" in fail("TODO")
//  		"start weighing job" in fail("TODO")
//  		"accept Abort message even though not busy" in fail("TODO")
//  	}
//  	"when Working" - {
//  		"abort whatever it was working on" in fail("TODO")
//  		"accept new job by aborting the current one and queuing the next"  in fail("TODO")
//  		"report success and return to Idle" in fail("TODO")
//  		"report failure and return to idle" in fail("TODO")
//  	}
//  	"when waiting for abort confirmation" - {
//  		"accept new job by setting it as next in queue" in fail("TODO")
//  		"accept redundant abort message" in fail("TODO") //Or better to thro exception?  Probably not, but at least log warning?
//  		"regard a results of aborted job as abort confirmation" in fail("TODO")
//  		"on abort confirmation" - {
//  			"start next job if known" in fail("TODO")
//  			"go to idle if next job not known" in fail("TODO")
//  		}
//  	}
//  }
//}

import org.scalatest.BeforeAndAfterAll
import org.scalatest.FreeSpecLike
import org.scalatest.mock.MockitoSugar
import akka.actor.ActorSystem
import akka.routing.Broadcast
import akka.testkit.TestFSMRef
import akka.testkit.TestKit
import akka.testkit.TestProbe
import sampler.abc.Model
import sampler.abc.config.ABCConfig
import sampler.abc.actor.sub.worker.AborterComponent
import sampler.abc.Prior
import sampler.abc.actor.sub.worker.Aborter
import org.mockito.Mockito.when
import sampler.abc.actor.sub.worker.ModelRunnerComponent
import sampler.abc.actor.sub.worker.WeigherComponentImpl
import sampler.abc.config.JobParameters
import sampler.abc.config.ClusterParameters
import sampler.abc.Population
import sampler.abc.Generation
import akka.dispatch.MessageDispatcher
//import sampler.abc.actor.main.component.WorkDispatcherComponent
import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config

class WorkerActorTest
    extends TestKit(ActorSystem("ABC"))
    with FreeSpecLike
    with MockitoSugar {
  type T = Int //Pretend model parameters
  val noMixing = 0l
  val hundredParticles = 100
  val threeGenerations = 3
  val isFinal = true
  val terminateAtTargetGen = true //TODO false

  case class TestParams()

  class TestableWorkerActor(
    //val config: ABCConfig)
      val config: Config)
      extends WorkerActor[T]
      with WeigherComponentImpl[T]
      with ModelRunnerComponent[T]
			//with WorkDispatcherComponent 
      with AborterComponent {
  //  self: AborterComponent =>
    val random = sampler.math.Random
    val model: Model[T] = mock[Model[T]]
    val prior = mock[Prior[T]]
    // when(model.prior).thenReturn(prior)
    val modelRunner = mock[ModelRunner]
    val aborter = new Aborter {}
    val workDispatcher = context.system.dispatchers.lookup("sampler.work-dispatcher")
    //val workDispatcher = context.system.dispatchers.lookup("akka.actor.default-dispatcher")
  }

  trait Setup {
    val abcConfig = ABCConfig(
      JobParameters(hundredParticles, 0, threeGenerations),
      null,
      ClusterParameters(terminateAtTargetGen, 0, 0l, 0, noMixing, 0l, 0l))
      
     val config = ConfigFactory.load("~/Sampler/sampler-abc/src/test/resources/application.conf")
           
//    val config = ConfigFactory.parseString(
//    """
//    | akka.actor {
//    |   default-dispatcher {
//    |      type = "Dispatcher"
//	  | executor = "fork-join-executor"
//	  | fork-join-executor {
//	  | 	parallelism-min = 2
//	  | 	parallelism-factor = 2
//	  | }
//	  | throughput = 1
//    |   }
//    | }
//    """.stripMargin)
      
    val instanceRef = TestFSMRef(new TestableWorkerActor(config))
    val instanceObj = instanceRef.underlyingActor

    val clientRef = TestProbe().ref
    var gen1: Generation[TestParams] = Population(mock[Map[TestParams, Double]], 1, 99)
    val prevPopulation: Generation[T] = Population(Map(1 -> 0.2, 2 -> 0.8), 0, 0)
    
    //val executionContext = mock[MessageDispatcher]
    //val executionContext = context.system.dispatchers.lookup("sampler.work-dispatcher")
    //val generateJob = GenerateParticlesFrom[T](prevPopulation, config)
  }

  "Worker actor should" - {
    //"start in Idle state with uninitialised data" in fail("TODO")
    "when Idle" - {
      "start generating job" in new Setup {
       // val routerProbe = TestProbe()

        instanceRef ! GenerateParticlesFrom[T](prevPopulation, abcConfig)

        // Assertions
        assertResult(Working)(instanceRef.stateName)

      }
//      "start weighing job" in new Setup {
//        //val routerProbe = TestProbe()
//
//        instanceRef ! WeighJob
//
//        // Assertions
//        assertResult(Working)(instanceRef.stateName)
//
//      }
//      "accept Abort message even though not busy" in new Setup {
//       // val routerProbe = TestProbe()
//
//        instanceRef ! Abort
//
//        // Assertions
//        assertResult(Abort)(instanceRef.stateName)
//
//      }
    }
//    "when Working" - {
//      "abort whatever it was working on" in fail("TODO")
//      "accept new job by aborting the current one and queuing the next" in fail("TODO")
//      "report success and return to Idle" in fail("TODO")
//      "report failure and return to idle" in fail("TODO")
//    }
//    "when waiting for abort confirmation" - {
//      "accept new job by setting it as next in queue" in fail("TODO")
//      "accept redundant abort message" in fail("TODO") //Or better to thro exception?  Probably not, but at least log warning?
//      "regard a results of aborted job as abort confirmation" in fail("TODO")
//      "on abort confirmation" - {
//        "start next job if known" in fail("TODO")
//        "go to idle if next job not known" in fail("TODO")
//      }
//    }
  }
}