package sampler.abc.actor

import akka.event.LoggingAdapter
import akka.actor.ActorLogging

/** Due to need to do dependency injection in the components e.g. ToleranceComponent.
 *  Hard-wiring of ActorLogging in FSM prevents this so we introduce a new logger
 */
trait LoggingAdapterComponent{
	val logg: LoggingAdapter
}

//TODO Sort out logging, so it works the same for actors or not

trait LoggingAdapterComponentImpl extends LoggingAdapterComponent{
	self: ActorLogging =>
	
	val logg = self.log
}