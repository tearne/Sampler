package sampler.abc.builder.local

import sampler.run.Abortable
import sampler.abc.ABCModel
import sampler.abc.ABCParameters
import sampler.abc.builder.ParticleBuilderComponent

trait JobBuilderComponent {
	self: ParticleBuilderComponent => 
	val jobBuilder: JobBuilder
	
	trait JobBuilder {
		def makeJobs(model: ABCModel)(abcParams: ABCParameters): Seq[Abortable[model.Population]]
	}
	
}