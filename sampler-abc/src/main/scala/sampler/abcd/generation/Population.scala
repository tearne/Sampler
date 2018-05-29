/*
 * Copyright (c) 2012-18 Crown Copyright
 *                       Animal and Plant Health Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.abcd.generation

import java.math.MathContext
import java.time.LocalDateTime

import play.api.libs.json._
import sampler._
import sampler.abcd._
import sampler.io.{Tokenable, Tokens}

case class Population[P](
    particles: Seq[Particle[P]],
    numParticlesRejected: Int,
    iteration: Int,
    tolerance: Double,
    uuid: Option[UUID]
) extends Generation[P] {

//  def winsOver[P](that: Population[P]) = {
//    assume(parentUUID.isDefined && that.uuid.isDefined, "Should not happen")
//
//    val meUUID = this.uuid.get
//    val themUUID = that.uuid.get
//
//    if(meUUID.timestamp != themUUID.timestamp){
//      // Older wins, since more particles are likely to have been generated from it
//      if(meUUID.timestamp < themUUID.timestamp) this else that
//    }
//    else {
//      if(meUUID.generatingNodeId < themUUID.generatingNodeId) this else that
//    }
//  }
//
//  def precedes(that: Population[P]): Boolean = {
//    (this.iteration < that.iteration) && (this.tolerance <= that.tolerance)
//  }

  lazy val acceptanceRatio: Double =
    particles.size.toDouble / (particles.size + numParticlesRejected)

  lazy val consolidatedWeightsTable: Map[P, Double] = {
    particles
        .groupBy(_.params)
        .map{case (k,v) => (k, v.map(_.weight).sum)}
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
    val mc = new MathContext(wtPrecision)
    val rows: Iterable[Tokens] = consolidatedWeightsTable.map{case (p, wt) =>
      tokenable.getTokens(p) + Tokens.named("weight" ->  BigDecimal(wt, mc))
                                                             }
    val names = rows.head.map.keys
    val particlesValuesByParam = names.map{name => name -> rows.map(_.map(name))}.toMap
    val details: Seq[Map[String, JsValue]] = particles.map{wp =>
      Map(
        "s" -> JsArray(wp.repScores.map(d => JsNumber(d))),
        "w" -> JsNumber(wp.weight),
        "p" -> JsObject(tokenable.getTokens(wp.params).map)
      )
                                                                  }

    Json.obj(
      "meta" -> Map(
        "comment" -> "Weights are not normalised",
        "created" -> LocalDateTime.now.toString
      ),
      "generation" -> iteration,
      "num-particles-rejected" -> numParticlesRejected,
      "acceptance-ratio" -> acceptanceRatio,  //Only output for convenience
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
    val numParticlesRejected = (json \ "num-particles-rejected").as[Int]
    val tolerance = (json \ "tolerance").as[Double]

    val particles = {
      val posteriorDetails = (json  \ "particle-details").as[Seq[JsValue]]
      def particleParser(json: JsValue) = {
        val params = tokenParser((json \ "p").as[Map[String, JsValue]])
        val scores = (json \ "s").as[Seq[Double]]
        val weight = (json \ "w").as[Double]
        Particle(
          params,
          scores,
          weight,
          iteration,
          None,        // No UUID for particles loaded from file
          None         // No parent generation UUID for particles loaded from file
        )
      }
      posteriorDetails.map(jsBlock => particleParser(jsBlock))
    }

    Population(
      particles,
      numParticlesRejected,
      iteration,
      tolerance,
      None
    )
  }
}