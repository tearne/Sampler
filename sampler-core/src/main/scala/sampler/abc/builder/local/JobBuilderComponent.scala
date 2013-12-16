package sampler.abc.builder.local

import sampler.run.Abortable
import sampler.abc.ABCModel
import sampler.abc.parameters
import sampler.abc.builder.ParticleBuilderComponent
import sampler.io.Logging
import sampler.math.Random
import sampler.abc.EncapsulatedPopulation
import sampler.abc.Weighted
import sampler.abc.parameters.JobParameters
import sampler.abc.parameters.AlgorithmParameters
import sampler.abc.parameters.ABCParameters

trait JobBuilderComponent {
	self: ParticleBuilderComponent with Logging => 
	val jobBuilder: JobBuilder
	
	trait JobBuilder {
		def makeJobs[M <: ABCModel](ePop: EncapsulatedPopulation[M])(
				params: ABCParameters,
				tolerance: Double, 
				r: Random
		): Seq[Abortable[Seq[Weighted[ePop.model.ParameterSet]]]] = {
		  	//Run the building in parallel on local machine, by chunking
			val jobSizes = (1 to params.job.numParticles)
				.grouped(params.algorithm.particleChunkSize)
				.map(_.size).toList
				
			log.info(s"Tolerance = $tolerance, Local job sizes = $jobSizes")
			
			val jobs = jobSizes.map{quantity => Abortable{aborter =>
				particleBuilder.apply(ePop.model)(ePop.population, quantity, tolerance, aborter, params, r)
			}}
			
			jobs
		}
	}
	
}