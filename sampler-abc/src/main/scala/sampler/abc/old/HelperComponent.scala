package sampler.abc.old

import sampler.abc.actor.children.flushing.ToleranceCalculator
import sampler.abc.actor.root.{Helper, ParticleMixer}
import sampler.maths.Random

//TODO delete as prob no longer user ((mpl)
trait HelperCoponentImpl extends HelperComponent {
	this: MainActor[_] =>

	lazy val helper = new Helper(
			new ParticleMixer(),
			ToleranceCalculator,
			getters,
			Random)
}

trait HelperComponent {
  val helper: Helper
}

