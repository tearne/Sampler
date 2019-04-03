package sampler.abc

import play.api.libs.json.{JsNumber, _}
import sampler._
import sampler.distribution.Distribution
import sampler.io.Tokenable

sealed trait Generation[P] {
  val iteration: Int
  val tolerance: Double

  def proposalDistribution(model: Model[P]): Distribution[P]
}

case class UseModelPrior[P](tolerance: Double = Double.MaxValue) extends Generation[P] {
  val iteration = 0

  /*
   *  Model is an argument rather than in constructor so
   *  class can safely be serialised and used as message.
   */
  def proposalDistribution(model: Model[P]) = model.prior.distributionSupportChecked
}

case class Population[P](
    weightedParticles: Seq[Weighted[P]],
    iteration: Int,
    tolerance: Double,
    acceptanceRatio: Double
) extends Generation[P] {

  lazy val consolidatedWeightsTable: Map[P, Double] = {
    weightedParticles
        .groupBy(_.scored.params)
        .map { case (k, v) => (k, v.map(_.weight).sum) }
  }

  /*
   *  Model is an argument rather than in constructor so
   *  class can safely be serialised and used as message.
   *
   *  Note that perturbations might go out of prior range
   *  but will rejected later before the model is run
   */
  def proposalDistribution(model: Model[P]) =
    consolidatedWeightsTable.toDistribution.map(model.perturb)

  def toJSON(wtPrecision: Int = 8)(implicit tokenable: Tokenable[P]) = {
    //    val mc = new MathContext(wtPrecision)
    //    val rows: Iterable[Tokens] = consolidatedWeightsTable.map { case (p, wt) =>
    //      tokenable.getTokens(p) + Tokens.named("weight" -> BigDecimal(wt, mc))
    //    }
    //    val names = rows.head.map.keys
    //    val particlesValuesByParam = names.map { name => name -> rows.map(_.map(name)) }.toMap
    val details: Seq[Map[String, JsValue]] = weightedParticles.map { wp =>
      Map(
        "s" -> JsArray(wp.scored.scores.map(d => JsNumber(d))),
        "w" -> JsNumber(wp.weight),
        "p" -> JsObject(tokenable.getTokens(wp.scored.params).map)
      )
    }

    Json.obj(
      "note" -> "Weights are not normalised",
      "generation" -> iteration,
      "acceptance-ratio" -> acceptanceRatio,
      "tolerance" -> tolerance,
      //      "particle-summary" -> particlesValuesByParam,
      "particle-details" -> details
    )
  }
}

object Population {
  def fromJson[T](jsonStr: String, tokenParser: Map[String, JsValue] => T): Population[T] = {
    val json = Json.parse(jsonStr)

    val iteration = (json \ "generation").as[Int]
    val acceptance = (json \ "acceptance-ratio").as[Double]
    val tolerance = (json \ "tolerance").as[Double]

    val weightedParticles = {
      val posteriorDetails = (json \ "particle-details").as[Seq[JsValue]]

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