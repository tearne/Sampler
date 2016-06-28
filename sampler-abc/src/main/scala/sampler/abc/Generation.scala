package sampler.abc

import sampler.math.Random
import play.api.libs.json._
import sampler.io.Rounding
import play.api.libs.json.Writes
import play.api.libs.json.JsNumber
import java.math.MathContext
import sampler.io.Tokenable
import sampler.io.Tokens
import java.util.Calendar
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import sampler.distribution.Distribution
import sampler._

sealed trait Generation[P]{
	val iteration: Int
	val tolerance: Double
	def proposalDistribution(model: Model[P], rnd: Random): Distribution[P]
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
	  weightedParticles: Seq[Weighted[P]],
	  iteration: Int, 
		tolerance: Double,
		acceptanceRatio: Double
) extends Generation[P]{
	
  lazy val consolidatedWeightsTable: Map[P, Double] = {
    weightedParticles
		  .groupBy(_.scored.params)
		  .map{case (k,v) => (k, v.map(_.weight).sum)}
  }
  
	/*
	 *  Model & Random are args rather than in constructor so 
	 *  this class can safely be serialised and used as message.
	 */
  // TODO what about the random?
	def proposalDistribution(model: Model[P], rnd: Random) =
	  consolidatedWeightsTable.toDistribution.map(model.perturb)
	
	def toJSON(wtPrecision: Int = 8)(implicit tokenable: Tokenable[P]) = {
		val mc = new MathContext(wtPrecision)
		val rows: Iterable[Tokens] = consolidatedWeightsTable.map{case (p, wt) => 
			tokenable.getTokens(p) + Tokens.named("weight" ->  BigDecimal(wt, mc))
		}
		val names = rows.head.map.keys
		val particlesValuesByParam = names.map{name => name -> rows.map(_.map(name))}.toMap
		val details: Seq[Map[String, JsValue]] = weightedParticles.map{wp =>
		  Map(
		    "s" -> JsArray(wp.scored.scores.map(d => JsNumber(d))),
		    "w" -> JsNumber(wp.weight),
		    "p" -> JsObject(tokenable.getTokens(wp.scored.params).map)
		  )
		}
		
		Json.obj(
		  "meta" -> Map(
				  "comment" -> "Weights are not normalised",
				  "created" -> LocalDateTime.now.toString        
		  ),
	    "generation" -> iteration,
  		"acceptance-ratio" -> acceptanceRatio,
  		"tolerance" -> tolerance,
  		"particle-summary" -> particlesValuesByParam,
		  "particle-details" -> details
		)
	}
}

object Population{
  def fromJson[T](jsonStr: String, tokenParser: Map[String, JsValue] => T): Population[T] = {
    val json = Json.parse(jsonStr)
    
    val iteration = (json \ "generation").as[Int]
    val acceptance = (json \ "acceptance-ratio").as[Double]
    val tolerance = (json \ "tolerance").as[Double]
    
    val weightedParticles = {
      val posteriorDetails = (json  \ "particle-details").as[Seq[JsValue]]
      def particleParser(json: JsValue): Weighted[T] = {
    	  val params = tokenParser((json \ "p").as[Map[String, JsValue]])
        val scores = (json \ "s").as[Seq[Double]]
        val weight = (json \ "w").as[Double]
        Weighted(
          Scored(params, scores, None), //'None' because we don't want these to mix
          weight
        )
      }
      posteriorDetails.map(jsBlock => particleParser(jsBlock))
    }
    
    Population(
      weightedParticles,
      iteration,
      tolerance,
      acceptance
    )
  }
}