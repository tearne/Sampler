package sampler.cluster.abc.actor

import akka.event.LoggingAdapter
import akka.actor.ActorLogging

trait LoggingAdapterComponent{
	val logSomething: LoggingAdapter
}

trait LoggingAdapterComponentImpl extends LoggingAdapterComponent{
	self: ActorLogging =>
	  
	 val logSomething = self.log
}