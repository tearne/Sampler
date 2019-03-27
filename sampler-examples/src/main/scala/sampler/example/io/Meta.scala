package sampler.example.io

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import play.api.libs.json.Json

object Meta extends App {

  import sampler._

  val inputConfig = {
    val  pretendConfig ="""
      myApplication {
        meta = [
          {
            project = myProject
            note = this was in a hocon config
            task = clean the data
            date = 1/4/1980
          }
        ]
      }

      notMyApplication {
        recipe {
           eggs = 4
           flour = 220
           milk = 430
           water = 120
        }
      }
    """

    ConfigFactory.parseString(pretendConfig).getConfig("myApplication")
  }

  // Our application would now run and our basic JSON outputs would be produced

  val basicOutputs = {
    val pretendOutputs = """
      {
        "parameters" : {
          "alpha" : 2,
          "beta" : 0.01,
          "gamma" : 7.2
        }
      }
    """
    Json.parse(pretendOutputs)
  }

  // Now augment the JSON outputs with some metadata
  val outputsWithMeta = basicOutputs
        .addSystemMeta()  // User, machine, date, class name etc.
        .addProject("Sampler examples")
        .addTask("Demonstrate adding meta to json")
        // Want to record the lineage by preserving the meta from out input config file
        .addHistoricMetaFrom(
          // In this case our config was HOCON/TypeSafe-Config, so we need to render it to JSON first
          Json.parse(inputConfig.root.render(ConfigRenderOptions.concise()))
        )
      .build

  // We'd now write this to disk
  println(Json.prettyPrint(outputsWithMeta))
}
