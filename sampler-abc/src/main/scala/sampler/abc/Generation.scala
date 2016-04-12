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
import sampler.io.Tokenable
import sampler.io.Tokens
import java.util.Calendar
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import play.api.libs.json.JsArray

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
		tolerance: Double,
		acceptanceRatio: Double
) extends Generation[P]{
	
	/*
	 *  Model & Random are args rather than in constructor so 
	 *  this class can safely be serialised and used as message.
	 */
	def proposalDistribution(model: Model[P], rnd: Random) = 
		DistributionBuilder
		.fromWeightsTable(particleWeights)(rnd)
		.map(model.perturb)
	
	def toJSON(wtPrecision: Int = 8)(implicit tokenable: Tokenable[P]) = {
		val mc = new MathContext(wtPrecision)
		val rows: Iterable[Tokens] = particleWeights.map{case (p, wt) => 
			tokenable.getTokens(p) + Tokens.named("weight" ->  BigDecimal(wt, mc))
		}
		val names = rows.head.map.keys
		val particlesValuesByParam = names.map{name => name -> rows.map(_.map(name))}.toMap
		
		Json.obj(
		  "meta" -> Map(
				  "comment" -> "Weights are not normalised",
				  "created" -> LocalDateTime.now.toString        
		  ),
			"generation" -> iteration,
			"acceptance-ratio" -> acceptanceRatio,
			"tolerance" -> tolerance,
			"particles" -> particlesValuesByParam
		)
	}
}

object Population{
  def fromJson[T](jsonStr: String, tokenParser: Map[String, JsValue] => T): Population[T] = {
    val json = Json.parse(jsonStr)
    
    val iteration = (json \ "generation").as[Int]
    val acceptance = (json \ "acceptance-ratio").as[Double]
    val tolerance = (json \ "tolerance").as[Double]
    
    val weightsByParticle = {
      val particleArrays = (json \ "particles")
        .as[JsObject]
        .fields
        .toMap
        
      val weights = particleArrays("weight").as[Seq[Double]]
      val paramTokens = (particleArrays - "weight")
        .mapValues(_.as[IndexedSeq[JsValue]])
      
      val numParams = paramTokens.head._2.size
      val paramTokenSets = 
        for(i <- 0 until numParams)
        yield paramTokens.map{case (name, valueIndexedSeq) => name -> valueIndexedSeq(i)}
      
      val particles = paramTokenSets.map(tokenParser)
      
      particles.zip(weights).toMap
    }
    
    Population(
      weightsByParticle,
      iteration,
      tolerance,
      acceptance
    )
  }
}