package sampler.abc

import sampler.data.Distribution
import sampler.math.Random
import sampler.data.DistributionBuilder
import play.api.libs.json.{JsNull,Json,JsString,JsValue}
import play.api.libs.json.JsObject
import sampler.io.Rounding
import play.api.libs.json.Writes
import play.api.libs.json.JsNumber

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

//TODO this is too hacky
//case class Token(value: String)
//object Token{
//	implicit val tokenWrites: Writes[Token] = new Writes[Token]{
//		def writes(t: Token) = JsNumber(BigDecimal(t.toString))
//	}
//}


trait Token{
	def get(): JsValue
}
object Token{
	implicit class RichDouble(d: Double){
		def toToken() = BigDecimalToken(BigDecimal(d))
		def toToken(decimalPlaces: Int) = BigDecimalToken(BigDecimal(d).setScale(decimalPlaces, BigDecimal.RoundingMode.HALF_UP))
	}
	
//	implicit def fromDouble(d: Double): BigDecimalToken = BigDecimalToken(BigDecimal(d))
//	implicit def fromBigDecimal(bd: BigDecimal): BigDecimalToken = BigDecimalToken(bd)
}
case class BigDecimalToken(bd: BigDecimal) extends Token{
	def get = JsNumber(bd)
}

trait Tokenable[T]{
	def namedTokens(data: T): Map[String, Token]
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
	
	def toJSON(weightDecimalPlaces: Int = 6)(implicit tokable: Tokenable[P]) = {
		import Token._
		val rowsAsMaps: Iterable[Map[String, Token]] = particleWeights.map{case (p, wt) => 
			tokable.namedTokens(p) + ("weight" ->  wt.toToken(weightDecimalPlaces))
		}
		val names = rowsAsMaps.head.keys
		val particlesValuesByParam = names.map{name => name -> rowsAsMaps.map(_(name))}.toMap
		
		implicit val writes = new Writes[Iterable[Token]]{
			def writes(tokens: Iterable[Token]) = Json.toJson(tokens.map(_.get))
		}
		
		Json.obj(
			"comment" -> "Weights are not normalised",
			"iteration" -> iteration,
			"tolerance" -> tolerance,
			"particles" -> particlesValuesByParam
		)
	}
}