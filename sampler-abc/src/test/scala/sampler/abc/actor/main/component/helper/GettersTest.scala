package sampler.abc.actor.main.component.helper

import org.scalatest.FreeSpec
import sampler.abc.config.ABCConfig
import sampler.abc.config.ClusterParameters
import sampler.abc.config.JobParameters

class GettersTest extends FreeSpec {
  "Getters should" - {
  	val instance = new Getters()
  	val config = ABCConfig(
				JobParameters(
					50000,
					0,
					0
				),
				null,
				ClusterParameters(
						false,
						0,
						0,
						0,
						5000,
						0
				)
		)
  	
  	"Get mix rate out of config" in {
  		assert(instance.getMixRateMS(config) === 1000l)
  	}
  	
  	"Get the required number of particles out of config" in {
  		assert(instance.getNumParticles(config) === 50000)
  	}
  }
}