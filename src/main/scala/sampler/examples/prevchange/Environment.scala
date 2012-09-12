package sampler.examples.prevchange

import java.nio.file.Paths
import sampler.math.Random

trait Environment {
	val workingDir = Paths.get("examples").resolve("PrevalenceChange")
	implicit val r = new Random()
}