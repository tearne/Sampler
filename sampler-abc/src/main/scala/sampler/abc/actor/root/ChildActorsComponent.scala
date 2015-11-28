package sampler.abc.actor.root

import akka.actor.Actor
import akka.actor.Props
import akka.routing.FromConfig
import sampler.abc.actor.BroadcastActor
import sampler.abc.actor.ReceiveActor
import sampler.abc.actor.worker.WorkerActorImpl
import sampler.abc.config.ABCConfig
import sampler.abc.Model
import sampler.abc.actor.ReportingActor
import sampler.abc.actor.FlushingActor

trait ChildrenActorsComponentImpl[P] extends ChildrenActorsComponent[P]{
	this: Actor with ABCActor[P] =>
	
	val childActors = new ChildActors{}
}

trait ChildrenActorsComponent[P] {
	this: Actor with ABCActor[P] =>
		
	val childActors: ChildActors

	trait ChildActors {
		val broadcaster = context.actorOf(
			Props(classOf[BroadcastActor], config), 
			"broadcaster"
		)
		
		val receiver = context.actorOf(
			Props[ReceiveActor], 
			"receiver"
		)
		
		val router = context.actorOf(
//		Props(new WorkerActorImpl[P](model)).withRouter(FromConfig()),// Akka 2.2.3
			FromConfig.props( 			// Akka 2.3
					Props(new WorkerActorImpl[P](model))
			),
			"work-router"
		)
		
		val flusher = context.actorOf(
			Props(classOf[FlushingActor[P]],algorithm),
			"flusher"
		)
		
		val reporter = context.actorOf(
			Props(classOf[ReportingActor[P]], reportAction)	
		)
	}
}