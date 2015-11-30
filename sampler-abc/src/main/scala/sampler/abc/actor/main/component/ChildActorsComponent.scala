package sampler.abc.actor.main.component

import akka.actor.Actor
import akka.actor.Props
import akka.routing.FromConfig
import sampler.abc.actor.sub.WorkerActorImpl
import sampler.abc.actor.sub.FlushingActor
import sampler.abc.actor.sub.BroadcastActor
import sampler.abc.actor.sub.ReceiveActor
import sampler.abc.actor.sub.ReportingActor
import sampler.abc.actor.main.MainActor
import sampler.abc.actor.sub.flushing.GenerationFlusher
import sampler.abc.core.ToleranceCalculator
import sampler.abc.actor.sub.flushing.ObservedIdsTrimmer
import sampler.abc.core.WeightsHelper

trait ChildActorsComponentImpl[P] extends ChildActorsComponent[P]{
	this: Actor with MainActor[P] =>
	
	lazy val childActors = new ChildActors{}
	val generationFlusher = new GenerationFlusher(
			ToleranceCalculator,
			new ObservedIdsTrimmer(
					config.cluster.particleMemoryGenerations, 
					config.job.numParticles),
			new WeightsHelper(),
			getters,
			config.job.numParticles)
}

trait ChildActorsComponent[P] {
	this: Actor with MainActor[P] =>
		
	val childActors: ChildActors
	val generationFlusher: GenerationFlusher

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
			Props(classOf[FlushingActor[P]], helper),
			"flusher"
		)
		
		val reporter = context.actorOf(
			Props(classOf[ReportingActor[P]], reportAction)	
		)
	}
}