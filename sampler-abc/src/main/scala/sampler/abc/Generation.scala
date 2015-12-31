package sampler.abc

import sampler.data.Distribution
import sampler.math.Random
import sampler.data.DistributionBuilder
import play.api.libs.json.{JsNull,Json,JsString,JsValue}
import play.api.libs.json.JsObject
import sampler.io.Rounding
import play.api.libs.json.Writes
import play.api.libs.json.JsNumber
import java.math.MathContext

sealed trait Generation[P]{
	val iteration: Int
	val tolerance: Double
	def proposalDistribution(model: Model[P], rnd: Random):Distribution[P]
}

case class UseModelPrior[P](tolerance: Double = Double.MaxValue) extends Generation[P]{
	val iteration = 0
	
	/*
	 *  Model & Random are args rather than in constructor so 
	 *  this class can safely be serialised and used as message.
	 */
	def proposalDistribution(model: Model[P], rnd: Random) = model.prior
}

case class Population[P](
	  particleWeights: Map[P, Double],
	  iteration: Int, 
		tolerance: Double
) extends Generation[P]{
	
	/*
	 *  Model & Random are args rather than in constructor so 
	 *  this class can safely be serialised and used as message.
	 */
	def proposalDistribution(model: Model[P], rnd: Random) = 
		DistributionBuilder
		.fromWeightsTable(particleWeights)(rnd)
		.map(model.perturb)
	
	def sampleByWeight(num: Int, random: Random) = {
		val dist = DistributionBuilder.fromWeightsTable(particleWeights)(random)
		(1 to num).map(_ => dist.sample)
	}
	
	def toJSON(precision: Int = 8)(implicit tokenable: Tokenable[P]) = {
		val mc = new MathContext(precision)
		val rowsAsMaps: Iterable[NamedTokens] = particleWeights.map{case (p, wt) => 
			tokenable.namedTokens(p) + ("weight" ->  JsNumber(BigDecimal(wt, mc)))
		}
		val names = rowsAsMaps.head.toMap.keys
		val particlesValuesByParam = names.map{name => name -> rowsAsMaps.map(_.toMap(name))}.toMap
		
		Json.obj(
			"comment" -> "Weights are not normalised",
			"iteration" -> iteration,
			"tolerance" -> tolerance,
			"particles" -> particlesValuesByParam
		)
	}
}