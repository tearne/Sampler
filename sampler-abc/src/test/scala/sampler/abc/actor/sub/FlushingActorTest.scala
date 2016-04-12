package sampler.abc.actor.sub

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import akka.testkit.TestKit
import akka.actor.ActorSystem
import org.scalatest.FreeSpecLike

class FlushingActorTest extends TestKit(ActorSystem("ABC-test")) with FreeSpecLike{
	"When asked to flush particles returns the result to the sender" in {
		fail("TODO")
	}
	
	"Throws exception if somehow asked to do another flushing job while still working on previous" in {
		fail("TODO")
	}
}