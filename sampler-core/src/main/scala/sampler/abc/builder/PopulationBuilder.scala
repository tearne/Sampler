package sampler.abc.builder

import sampler.abc.ABCModel
import sampler.abc.EncapsulatedPopulation
import sampler.abc.parameters._
import sampler.math.Random

trait PopulationBuilder {
	/*
	 * Implement the run method using preferred execution method to build the population. 
	 * For example, the LocalPopulationBuilder just farms each of the jobSizes to the build
	 * method in this class.  The cluster implementation (see sampler-cluster) runs the
	 * build method at every node
	 */
	def run[M <: ABCModel](
			ePop: EncapsulatedPopulation[M], 
			parameters: ABCParameters,
			tolerance: Double,
			random: Random
	): Option[EncapsulatedPopulation[M]]
}