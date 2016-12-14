package sampler.example

import java.nio.file.{Files, Paths}

import org.apache.commons.io.FileUtils
import org.apache.commons.math3.distribution.NormalDistribution
import play.api.libs.json.{JsValue, Json}
import sampler._
import sampler.distribution.{Distribution, DistributionImplicits}
import sampler.empirical.EmpiricalImplicits
import sampler.io.Rounding
import sampler.maths.Random
import sampler.r.script.RScript
import sampler.samplable.{ConvergenceProtocol, MaxMetric, ParallelSampler}

/*
 *  Given an imperfect test characterised by empirical data, how many samples should be taken to
 *  achieve a certain sensitivity of the approach?
 *
 *  Empirical data on test performance is transformed into probability of detection when testing
 *  multiple infected replicates.  Sample size is increased until the desired sensitivity is
 *  achieved.
 */
object SampleSizeUncertainty extends App with Rounding with DistributionImplicits with EmpiricalImplicits {
	implicit val random = Random
	val wd = Paths.get("results", "SampleSize")
	Files.createDirectories(wd)

	val requiredSensitivity = 0.95

	/* Generate some fake performance data.  In general this may be generated externally
	 * and loaded, for example using the [[sampler.io.ChainReader]].
	 */
	val generatedData: Seq[Double] = {
		val normal = new NormalDistribution(0.7,0.3)
		Distribution.from(_ => normal.sample)
      .filter{(x: Double) => x > 0 && x < 1}
			.until(_.size == 1000)
			.sample
	}

	val empiricalDistribution = generatedData.toDistribution
	val empiricalMean = generatedData.toEmpirical.mean
	val meanDistribution = Distribution.always(empiricalMean)

	def getSampleSize(observedSe: Distribution[Double], requiredSensitivity: Double) = {

		// Transform the samples into a probability of detection given a sample size
		def testingModel(numSamples: Int) =
			observedSe.map{se =>
				random.nextBoolean(1 - math.pow((1 - se), numSamples))
			}

		// Determine the smallest sample size which provides the desired sensitivity
		def loop(currentSampleSize: Int = 1): Int = {
			val model = testingModel(currentSampleSize)

			val se = {
				val chunkSize = 1e4.toInt
				val tolerance = 1e-5
				val maxSamples = 1000000

				ParallelSampler(model, random){
					new ConvergenceProtocol[Boolean](chunkSize, tolerance, maxSamples)
					with MaxMetric
				}
					.toEmpirical
					.probabilityTable(true)
			}

			println(s" - Sample Size $currentSampleSize => Sensitivity ${se.decimalPlaces(3)}")

			if(se >= requiredSensitivity) currentSampleSize
			else loop(currentSampleSize + 1)
		}

		loop()
	}

  val json: JsValue = Json.obj(
    "PretendData" -> generatedData.map(_.decimalPlaces(3)),
    "Mean" -> empiricalMean
  )

	Files.createDirectories(wd)
  FileUtils.writeStringToFile(wd.resolve("json.json").toFile(), Json.prettyPrint(json))

	RScript("""
	  library(ggplot2)
	  library(rjson)

	  jsonRaw = fromJSON(file = "json.json")
	  jsonData = data.frame("samples" = jsonRaw$PretendData)

	  pdf("plot.pdf", width=8.26, height=2.91)

  	ggplot(jsonData, aes(x=samples)) +
  		geom_histogram() +
			ggtitle("Test Performance Data") +
			geom_vline(xintercept = jsonRaw$Mean, colour = 'red') +
			coord_cartesian(xlim = c(0, 1))


	  dev.off()
	  """,
		wd.resolve("plot.r")
	)

	println("Calculating sample size using full test performance data")
	val result1 = getSampleSize(empiricalDistribution, requiredSensitivity)
	//println(s"$result1 samples are required to achieve Se = $requiredSensitivity\n")

	println("Calculating sample size based on mean")
	val result2 = getSampleSize(meanDistribution, requiredSensitivity)
	//println(s"$result2 samples are required to achieve Se = $requiredSensitivity")
}
