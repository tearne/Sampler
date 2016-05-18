package sampler.abc.actor.main.component.helper

//import sampler.abc.Weighted
import sampler.abc.actor.main.WeighedParticles
import sampler.abc.actor.main.EvolvingGeneration
import com.typesafe.config.Config
import java.util.concurrent.TimeUnit.MILLISECONDS

class Getters {
	//TODO so far these are just used in logging, to avoid errors when testing, but they could
	// be used more widely, or perhaps converted to lenses?
	def getNumParticles[P](weighedParticles: WeighedParticles[P]): Int = weighedParticles.seq.size
	def getNumEvolvedParticles[P](gen: EvolvingGeneration[P]): Int = gen.weighed.size
		
//	def weighedParticlesWithoutIdTags[P](weighed: WeighedParticles[P]): Seq[Weighted[P]]
//		= weighed.seq.map(_.value)
}