package sampler.example

import java.nio.file.Paths

import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import sampler._
import sampler.distribution.Distribution
import sampler.maths.Random

object PiecewiseLinearDistribution extends App {
  val wDir = Paths.get("results", "PiecewiseLinearDist")

  val points = Seq(
    (0.1, 2.0),
    (0.2, 3.0),
    (0.3, 2.0),
    (0.4, 2.0),
    (0.5, 2.0),
    (0.9, 1.5),
    (1.0, 0.0)
  )

  val dist = Distribution.piecewiseLinear(points: _*)
  val samples = dist.until(_.size == 1e5).sample(Random)

  val jsonStr = Json.prettyPrint(Json.obj(
    "points" -> Json.obj(
      "x" -> points.map(_._1),
      "y" -> points.map(_._2)
    ),
    "samples" -> samples.map(_.significanatFigures(4))
  ))

  FileUtils.writeStringToFile(
    wDir.resolve("data.json").toFile,
    jsonStr
  )

  val script = """
lapply(c("ggplot2", "reshape2", "jsonlite"), library, character.only=T)
pdf("plot.pdf", width=4.13, height=2.91)

data = fromJSON("data.json")
points = data.frame(data$points)
samples = data.frame(sample = data$samples)

ggplot(points) +
  geom_line(aes(x, y)) +
  geom_density(data = samples, aes(sample), colour = "blue", adjust = 0.5)
"""

  sampler.r.script.RScript(script, wDir.resolve("script.R"))
}
