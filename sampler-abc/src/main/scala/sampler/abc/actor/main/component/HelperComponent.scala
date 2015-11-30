package sampler.abc.actor.main.component

import sampler.abc.actor.main.helper.Helper
import sampler.abc.actor.sub.flushing.GenerationFlusher
import sampler.abc.core.ToleranceCalculator
import sampler.abc.actor.sub.flushing.ObservedIdsTrimmer
import sampler.abc.core.WeightsHelper
import sampler.abc.actor.main.helper.ParticleMixer
import sampler.math.Random
import sampler.abc.actor.main.MainActor

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