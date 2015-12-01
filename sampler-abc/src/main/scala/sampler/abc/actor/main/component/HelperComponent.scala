package sampler.abc.actor.main.component

import sampler.abc.actor.main.component.helper.Helper
import sampler.abc.actor.sub.flushing.GenerationFlusher
import sampler.abc.actor.sub.flushing.ToleranceCalculator
import sampler.abc.actor.sub.flushing.ObservedIdsTrimmer
import sampler.abc.actor.sub.flushing.WeightsHelper
import sampler.math.Random
import sampler.abc.actor.main.MainActor
import sampler.abc.actor.main.component.helper.ParticleMixer

trait HelperCoponentImpl extends HelperComponent {
	this: MainActor[_] =>
	
	lazy val helper = new Helper(
			new ParticleMixer(),
			getters,
			Random)
}

trait HelperComponent {
  val helper: Helper
}