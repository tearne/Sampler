package sampler.abc

import java.nio.file.Path
import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import sampler.io.Tokenable

object StandardReport {
  def apply[Params: Tokenable](wd: Path): Population[Params] => Unit = {
    pop: Population[Params] => {
  		val json = Json.prettyPrint(pop.toJSON())
  		FileUtils.write(wd.resolve(s"Gen${pop.iteration}.json").toFile, json)
    }
	}
}