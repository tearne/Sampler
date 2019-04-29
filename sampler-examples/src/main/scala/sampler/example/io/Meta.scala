package sampler.example.io

import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import play.api.libs.json.{JsValue, Json}

object Meta extends App {

  import sampler._

  val config: Config = {
    val  json ="""
      myApplication {
        breadcrumbs = {
          kind = Program configuration
          project = myProject
          task = clean the data
          inputs = /shared/drive/project/in/data.csv
          outputs = /shared/drive/project/out
        }
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

    ConfigFactory.parseString(json).getConfig("myApplication")
  }

  val parameters: JsValue = {
    val json ="""{
      "breadcrumbs" : {
        "kind" : "model parameters",
        "project" : "AB123",
        "task" : "scenario: worst case spread",
        "date" : "2019-04-25",

        "parameters" : {
          "alpha" : 2,
          "beta" : 0.01,
          "gamma" : 7.2
        },

        "upstream" : [
          {
            "type" : "parameter source for alpha, beta, gamma",
            "source" : "Joe Bloggs",
            "date" : "2018-03-12"
          }
        ]
      }

    }"""
    Json.parse(json)
  }

  // Our application would now run and our basic JSON outputs would be produced

  val rawOutputs: JsValue = {
    val json = """
      {
        "outputValues" : [1,4,3,2,4,1,3,4,2,1,1,1]
      }
    """
    Json.parse(json)
  }

  // Now augment the JSON outputs with some metadata
  val outputsWithMeta =
    // Start with our raw output data
    rawOutputs
        // Add metadata due to this precessing step
        .addSystemMeta()    // Date, hostname, username, stack trace etc
        .addProject("Sampler examples")
        .addKind("Example model output")
        .addTask("Demonstrate adding meta to some outputs")
        // Now add upstream metadata ...
        // ... from the system config (which was HOCON, so must convert to JSON first)
        .addUpstream(Json.parse(config.root.render(ConfigRenderOptions.concise())))
        // ... from the parameters config
        .addUpstream(parameters)
        .build

  // We'd now write this to disk
  println(Json.prettyPrint(outputsWithMeta))
}
