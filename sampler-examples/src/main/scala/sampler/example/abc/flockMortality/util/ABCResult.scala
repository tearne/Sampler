package sampler.example.abc.flockMortality.util

import play.api.libs.json.Json
import play.api.libs.json.Writes
import sampler.abc.ABCConfig
import sampler.abc.Population

case class ABCResult(
    prior: IntervalPrior,
    observed: IndexedSeq[Observed],
    config: ABCConfig,
    population: Population[Parameters]
)
object ABCResult {
  
  implicit val resultWrites = new Writes[ABCResult] {
    def writes(data: ABCResult) = {
      val observedJSON = data.observed.map(shed => Json.toJson(shed))
      Json.obj(
        "prior" -> Json.toJson(data.prior),
        "observed" -> observedJSON,
        "config" -> Json.obj(
            "generations" -> data.config.numGenerations,
            "particles" -> data.config.numParticles
        ),
        "population" -> data.population.toJSON()
      )
    }
  }
  
}