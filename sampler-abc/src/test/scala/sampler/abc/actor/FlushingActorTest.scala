package sampler.abc.actor

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import akka.testkit.TestKit
import akka.actor.ActorSystem
import org.scalatest.FreeSpecLike

@RunWith(classOf[JUnitRunner])
class FlushingActorTest extends TestKit(ActorSystem("ABC")) with FreeSpecLike{
	"When asked to flush particles returns the result to the sender" in {
		pending
	}
	
	"Throws exception if somehow asked to do another flushing job while still working on previous" in {
		pending
	}
}