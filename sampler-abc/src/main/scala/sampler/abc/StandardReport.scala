package sampler.abc

import java.nio.file.Path

import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import sampler.io.{Meta, Tokenable}

object StandardReport extends Meta{
  def apply[Params: Tokenable](wd: Path, config: ABCConfig, prefix: String = "Gen"): Population[Params] => Unit = {
    pop: Population[Params] => {
      val json = pop.toJSON()
          .addSystemMeta
          .addTask("ABC run - intermediate generation produced with StandardReport")
          .addHistoricMetaFrom(config.asJson)
          .build

      val jsonStr = Json.prettyPrint(json)
  		FileUtils.write(wd.resolve(f"$prefix${pop.iteration}%03d.json").toFile, jsonStr)

    }
	}
}