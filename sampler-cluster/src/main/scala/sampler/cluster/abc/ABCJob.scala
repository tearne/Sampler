package sampler.cluster.abc

import scala.language.existentials
import sampler.abc.ABCModel
import sampler.abc.ABCParameters

case class ABCJob[P](
		population: P, 
		currentTolerance: Double, 
		abcParams: ABCParameters
)