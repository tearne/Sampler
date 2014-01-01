package sampler.cluster.abc.actor.root

import sampler.cluster.abc.config.ABCConfig
import sampler.cluster.abc.Model

trait ModelAndConfig[P] {
	val config: ABCConfig
	val model: Model[P]
}