package sampler.abc.old

import akka.actor.{Actor, Props}
import akka.routing.FromConfig
import sampler.abc.Population
import sampler.abc.actor.children._
import sampler.abc.actor.children.flushing.{GenerationFlusher, ObservedIdsTrimmer}
import sampler.maths.Random

trait ChildActorsComponentImpl[P] extends ChildActorsComponent[P]{
	this: Actor 
		with MainActor[P] 
		with HelperComponent =>
	
	lazy val childActors = new ChildActors{}
	lazy val numParticles = config.numParticles
	lazy val generationFlusher = new GenerationFlusher(
			helper.toleranceCalculator,
			new ObservedIdsTrimmer(
					config.memoryGenerations, 
					numParticles),
			getters,
			config)
	val random = Random
}

trait ChildActorsComponent[P] {
	this: Actor 
		with MainActor[P]
		with HelperComponent =>
		
	val childActors: ChildActors
	val generationFlusher: GenerationFlusher
	val random: Random
	val reportHandler: Option[Population[P] => Unit]

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
			FromConfig.props(
					Props(new WorkerActorImpl[P](model, random))
			),
			"work-router"
		)
		
		val flusher = context.actorOf(
			Props(classOf[FlushingActor[P]], generationFlusher),
			"flusher"
		)
		
		val reporter = context.actorOf(
			Props(classOf[ReportingActor[P]], reportHandler)	
		)
	}
}