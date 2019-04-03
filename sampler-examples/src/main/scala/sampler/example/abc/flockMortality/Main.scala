package flockMortalityExample

import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import play.api.libs.json.Json
import sampler._
import sampler.abc._
import sampler.distribution.Distribution
import sampler.example.abc.flockMortality.util.{Model, _}
import sampler.maths.Random

object Main extends App {

  implicit val r = Random

  // Name and location of output files
  val outDir = Paths.get("results", "flockMortality").toAbsolutePath
  Files.createDirectories(outDir)

  // Observed data
  val observedJsonString =
    """{
    "observed" : [ {
      "id" : 1,
      "size" : 3000,
      "days" : [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 ],
      "eggs" : [ 2400, 2400, 2400, 2400, 1561, 1283, 1097, 971, 888, 831, 794, 769, 752, 740, 733, 728, 724, 722, 720, 719, 719, 718, 718, 718, 717 ],
      "dead" : [ 0, 0, 0, 0, 89, 63, 43, 29, 19, 13, 9, 6, 4, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      "infectionFreeDays" : 3
    }, {
      "id" : 2,
      "size" : 3000,
      "days" : [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 ],
      "eggs" : [ 2400, 2400, 2400, 2400, 2400, 2400, 2400, 1561, 1283, 1097, 971, 888, 831, 794, 769, 752, 740, 733, 728, 724, 722, 720, 719, 719, 718 ],
      "dead" : [ 0, 0, 0, 0, 0, 0, 0, 89, 63, 43, 29, 19, 13, 9, 6, 4, 3, 2, 1, 0, 0, 0, 0, 0, 0 ],
      "infectionFreeDays" : 6
    } ]
  }"""
  val observedJson = Json.parse(observedJsonString)
  val observed = Observed(observedJson)

  /*
   * beta = transmission rate
   * eta = 1 / latent period
   * gamma = 1 / infectious period
   * delta = mortality rate
   * sigma = rate of egg production for infectious birds
   * sigma2 = rate of egg production for recovered birds
   * offset = start day of infection
   */

  // Prior
  val priorJsonString =
    """{
      "type" : "interval",
      "params" : {
        "beta" : [ 0.0, 0.5 ],
        "eta" : [ 0.0, 1.0 ],
        "gamma" : [ 0.1, 1.0 ],
        "delta" : [ 0.0, 1.0 ],
        "sigma" : [ 0.1, 0.8 ],
        "sigma2" : [ 0.1, 0.8 ],
        "offset" : [ -5, 15 ]
      }
  }"""
  val priorJson = Json.parse(priorJsonString)
  val prior = IntervalPrior(priorJson)

  // Create an instance of Model based on the observed data and prior
  val model = new Model(observed, prior)

  // Load flock mortality parameters from application.conf
  val abcConfig = ABCConfig(ConfigFactory.load.getConfig("flock-mortality-example"))

  val abcReporting = StandardReport[Parameters](outDir, abcConfig)

  // Use ABC to produce population of parameters
  val population: Population[Parameters] = ABC(model, abcConfig, abcReporting)
  //  JSON.writeToFile(outDir.resolve("population.json"), population.toJSON())

  StandardReport.doPlotting(outDir)

  //  val result = ABCResult(prior, observed, abcConfig, population)
  //  val resultJSON = Json.toJson(result)
  //  JSON.writeToFile(resultsJSON, resultJSON)

  //=======================
  // SAMPLE FROM POPULATION TO PRODUCE POSTERIOR

  println("Sample from population to produce posterior: ")

  val numParticles = abcConfig.numGenerations
  val sample: IndexedSeq[Parameters] = Distribution.fromWeightsTable(population.consolidatedWeightsTable).until(_.size == 1000 * numParticles).sample
  val posterior: Posterior = Posterior.fromSeq(sample)

  println(posterior.offset.length)

  //=======================
  // POSTERIOR FIT

  println("Observed data: ")
  println("Dead: " + observed.map(_.dead))
  println("Eggs: " + observed.map(_.eggs))

  println("Fitting median parameters: ")

  val medianParams = Posterior.getMarginalMedian(posterior)
  //  println(medianParams)

  val modelFit = FittedResult(model.modelDistribution(medianParams).sample)
  println(modelFit.map(_.fitDead))
  println(modelFit.map(_.fitEggs))

  // Convert list of Fitted data to jsons
  val jsonFit = modelFit.map { shed =>
    val json = Json.toJson(shed)
    val filename = s"Shed${shed.id}_fitted.json"
    JSON.writeToFile(outDir.resolve(filename), json)
    json
  }

  println("Fitted parameters: " + medianParams.toSeq)

  // Call PlotResult.scala if you want to see the output - the ABC will need to have finished and generated result.json
}