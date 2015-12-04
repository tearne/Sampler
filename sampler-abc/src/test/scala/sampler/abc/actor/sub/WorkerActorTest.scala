package sampler.abc.actor.sub

import org.scalatest.FreeSpecLike
import akka.testkit.TestKit
import akka.actor.ActorSystem

class WorkerActorTest extends TestKit(ActorSystem("ABC-test")) with FreeSpecLike{
  "Worker actor should" - {
  	"start in Idle state with uninitialised data" in fail("TODO")
  	"when Idle" - {
  		"start generating job" in fail("TODO")
  		"start weighing job" in fail("TODO")
  		"accept Abort message even though not busy" in fail("TODO")
  	}
  	"when Working" - {
  		"abort whatever it was working on" in fail("TODO")
  		"accept new job by aborting the current one and queuing the next"  in fail("TODO")
  		"report success and return to Idle" in fail("TODO")
  		"report failure and return to idle" in fail("TODO")
  	}
  	"when waiting for abort confirmation" - {
  		"accept new job by setting it as next in queue" in fail("TODO")
  		"accept redundant abort message" in fail("TODO") //Or better to thro exception?  Probably not, but at least log warning?
  		"regard a results of aborted job as abort confirmation" in fail("TODO")
  		"on abort confirmation" - {
  			"start next job if known" in fail("TODO")
  			"go to idle if next job not known" in fail("TODO")
  		}
  	}
  }
}