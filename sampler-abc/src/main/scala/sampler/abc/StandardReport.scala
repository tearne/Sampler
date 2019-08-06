package sampler.abc

import java.io.InputStream
import java.nio.file.Path

import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import sampler.io.{Meta, Tokenable}
import sampler.r.script.RScript

import scala.io.Source

object StandardReport extends Meta {
  def apply[Params: Tokenable](wd: Path, config: ABCConfig, prefix: String = "Gen", task: String = "ABC generation report"): Population[Params] => Unit = {
    pop: Population[Params] => {
      val json = pop.toJSON()
          .addSystemMeta
          .addTask(task)
          .addUpstream(config.asJson)
          .build

      val jsonStr = Json.prettyPrint(json)
      FileUtils.write(wd.resolve(f"$prefix${pop.iteration}%03d.json").toFile, jsonStr)
    }
  }

  // TODO should either pass the prefix to the plot or not allow user to change it
  def doPlotting(outDir: Path): Unit = {
    // This is not supported in our cross build for Scala 2.11...
    //val scriptAsLines = Source.fromResource("posteriorPlot.r").getLines()
    // ... so using this version used instead.
    val stream: InputStream = getClass.getResourceAsStream("posteriorPlot.r")
    val scriptAsLines: Iterator[String] = Source.fromInputStream( stream ).getLines

    val lineSep = System.lineSeparator()
    val script = scriptAsLines.mkString(lineSep)
    RScript(script, outDir.resolve("posteriorPlot.r"))
  }
}