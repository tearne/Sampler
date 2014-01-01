package sampler.cluster.abc.actor.root

import akka.actor.Actor
import akka.actor.Props
import akka.routing.FromConfig
import sampler.cluster.abc.actor.BroadcastActor
import sampler.cluster.abc.actor.ReceiveActor
import sampler.cluster.abc.actor.worker.WorkerActorImpl
import sampler.cluster.abc.config.ABCConfig
import sampler.cluster.abc.Model

trait ChildrenActorsComponent[P] {
	this: Actor with ModelAndConfig[P] =>
		
	val childrenActors: ChildrenActors

	trait ChildrenActors {
		val broadcaster = context.actorOf(
			Props(classOf[BroadcastActor], config), 
			"broadcaster"
		)
		val receiver = context.actorOf(
			Props[ReceiveActor], 
			"receiver"
		)
		val workerRouter = context.actorOf(
			FromConfig.props(Props(new WorkerActorImpl[P](model))), 
			"work-router"
		)
	}
}