package sampler.io

import java.time.LocalDateTime

import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import play.api.libs.json._

import scala.sys.process._

object Meta extends Meta

trait Meta {

  trait MetaBuilderTrait {
    def validateMetaArray(json: JsValue): Unit = {

      val path = (__ \ "meta").json.pick[JsArray]
      assume(json.validate(path).isSuccess, s"No meta block found in $json")
    }

    def update(name: String, value: String): MetaBuilder

    def addSystemMeta() = update("date", LocalDateTime.now.toString)
        .update("hostname", "hostname".!!.takeWhile(_ != System.lineSeparator.charAt(0)))
        .update("username", System.getProperty("user.name"))
        .update("class", Thread.currentThread.getStackTrace()(4).getClassName)

    def addProjectMeta(project: String): MetaBuilder = update("project", project)

    def addTaskMeta(name: String) = update("task", name)
  }

  implicit class JsonMeta(json: JsValue) extends MetaBuilderTrait {
    def update(key: String, value: String): MetaBuilder = {
      validateMetaArray(json)
      MetaBuilder(Json.obj(key -> value), json)
    }
  }

  implicit class HOCONMeta(config: Config) extends MetaBuilderTrait {
    def update(key: String, value: String): MetaBuilder = {
      val json = Json.parse(config.root.render((ConfigRenderOptions.concise)))
      validateMetaArray(json)
      MetaBuilder(Json.obj(key -> value), json)
    }
  }

  case class MetaBuilder(metaMap: JsObject, json: JsValue) extends MetaBuilderTrait {
    def update(key: String, value: String) = copy(metaMap = metaMap.+(key -> JsString(value)))

    private val updater: Reads[JsObject] = (__ \ "meta").json.update(
      __.read[JsArray].map(a => a.append(metaMap))
    )

    def build() = json.transform(updater).get
  }
}


object Example extends App {
  import sampler._

  val inHOCON ="""
    myApplication {
      meta = [
        {
          project = myProject
          task = clean the data
          date = 1/4/1980
        }
      ]
    }

    pancakes {
       eggs = 4
       flour = 220
       milk = 430
       water = 120
    }
"""

  println(Json.prettyPrint(
    ConfigFactory.parseString(inHOCON).getConfig("myApplication")
        .addSystemMeta()
        .addProjectMeta("My Project 2")
        .addTaskMeta("Demonstrate augmenting meta with HOCON files")
        .build
  ))

  println(Json.prettyPrint(
    ConfigFactory.parseString(inHOCON).getConfig("pancakes")
        .addSystemMeta()
        .addProjectMeta("My Project 3")
        .addTaskMeta("Demonstrate adding meta when the block doesn't already exist")
        .build
  ))


  val inStr = """
  {
    "meta" : [
    {
      "date": "1/2/3",
      "application": "my upstream applicaiton",
      "task" : "pre-processing movement data"
    }
    ]
  }
"""

  println(
    Json.prettyPrint(
      Json.parse(inStr)
          .addSystemMeta()
          .addProjectMeta("Updated project")
          .addTaskMeta("Demonstrate adding meta to json")
          .build
    )
  )
}

