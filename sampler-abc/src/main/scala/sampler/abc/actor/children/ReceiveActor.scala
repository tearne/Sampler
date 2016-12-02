package sampler.abc.actor.children

import akka.actor.Actor

/*
 * This actor receives payloads from other actors and forwards them
 * to the root actor for processing.  In the application.conf it is
 * set up with it's own pinned dispatcher, to ensure that it will
 * be responsive even when the system is under load, thus helping to
 * ensure that heartbeat messages are successful.
 */
class ReceiveActor extends Actor {
  def receive = {
    case msg => context.parent.forward(msg)
  }
}