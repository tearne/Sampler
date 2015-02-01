package sampler.cluster.abc.actor.root

import akka.actor.Actor
import akka.actor.Props
import akka.routing.FromConfig
import sampler.cluster.abc.actor.BroadcastActor
import sampler.cluster.abc.actor.ReceiveActor
import sampler.cluster.abc.actor.worker.WorkerActorImpl
import sampler.cluster.abc.config.ABCConfig
import sampler.cluster.abc.Model
import sampler.cluster.abc.actor.ReportingActor

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
//			Props(new WorkerActorImpl[P](model)).withRouter(FromConfig()),	// Akka 2.2.3
			FromConfig.props(Props(new WorkerActorImpl[P](model))), 		// Akka 2.3
			"work-router"
		)
		
		val reportingActor = context.actorOf(
			Props(
				classOf[ReportingActor[P]], 
				reportAction
			)	
		)
	}
}