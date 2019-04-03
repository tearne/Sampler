package sampler.abc

import java.nio.file.Path

import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import sampler.io.{Meta, Tokenable}
import sampler.r.script.RScript

import scala.io.Source

object StandardReport extends Meta {
  def apply[Params: Tokenable](wd: Path, config: ABCConfig, prefix: String = "Gen"): Population[Params] => Unit = {
    pop: Population[Params] => {
      val json = pop.toJSON()
          .addSystemMeta
          .addTask("ABC generation report")
          .addHistoricMetaFrom(config.asJson)
          .build

      val jsonStr = Json.prettyPrint(json)
      FileUtils.write(wd.resolve(f"$prefix${pop.iteration}%03d.json").toFile, jsonStr)
    }
  }

  def doPlotting(outDir: Path): Unit = {
    val scriptAsLines = Source.fromResource("posteriorPlot.r").getLines()
    val lineSep = System.lineSeparator()
    val script = scriptAsLines.mkString(lineSep)
    RScript(script, outDir.resolve("posteriorPlot.r"))
  }
}