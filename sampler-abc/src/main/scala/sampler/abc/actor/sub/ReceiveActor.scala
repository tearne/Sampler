package sampler.abc.actor.sub

import akka.actor.Actor
import akka.actor.ActorLogging

/*
 * This actor exists to 
 * a) receive payloads from other cluster nodes and forward them to
 *    the root actor for processing.
 * b) ensure fast response time to Identify messages used for network
 *    load testing.  Using the root actor directly would introduce
 *    unnecessary lag as it processes payloads.
 */
class ReceiveActor extends Actor with ActorLogging {
	def receive = {
		case msg => 
		  context.parent.forward(msg)
	}
}