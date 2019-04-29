package sampler.io

import java.time.LocalDateTime

import play.api.libs.json._

import scala.sys.process._

object Meta extends Meta

trait Meta {

  implicit class JsonMeta(json: JsValue) {
    def addSystemMeta(): MetaBuilder = {
      MetaBuilder.init(json).addSystemMeta()
    }

    def add(key: String, value: JsValue): MetaBuilder = {
      MetaBuilder.init(json).add(key, value)
    }

    def addUpstreamFrom(arr: JsValue): MetaBuilder = {
      MetaBuilder.init(json).addUpstream(json)
    }
  }


  case class MetaBuilder(metaMap: JsObject, json: JsValue, upstream: JsArray = JsArray.empty) {
    def add(key: String, value: String): MetaBuilder = add(key, JsString(value))
    def add(key: String, value: JsValue): MetaBuilder = copy(metaMap = metaMap.+(key -> value))

    def addSystemMeta() = add("date", LocalDateTime.now.toString)
        .add("hostname", "hostname".!!.takeWhile(_ != System.lineSeparator.charAt(0)))
        .add("username", System.getProperty("user.name"))
        .add("simplified-stack", {
          val distinctStringMatcher: PartialFunction[Seq[String], String] = {
            case Seq(a, b) if a != b => b
          }

          val simpleStack: Seq[JsString] = Thread.currentThread
              .getStackTrace
              .toSeq
              .map(_.getClassName.takeWhile(_ != '$'))
              .sliding(2)
              .collect(distinctStringMatcher)
              .map(JsString)
              .toSeq

          JsArray(simpleStack)
        })

    def addProject(project: String): MetaBuilder = add("project", project)

    def addTask(name: String): MetaBuilder = add("task", name)

    def addKind(text: String): MetaBuilder = add("kind", text)

    def addUpstream(json: JsValue): MetaBuilder = {
      val breadcrumbPicker = (__ \ "breadcrumbs").json.pick[JsObject]

      json.validate(breadcrumbPicker).fold(
        // TODO, warn, don't fail
        invalid = _ => throw new AssertionError("No breadcrumbs found in provided document: "+json),
        valid = breadcrumbs => this.copy(upstream = upstream.append(breadcrumbs))
      )
    }

    def build(): JsValue = {
      val allMeta = metaMap.+("upstream", upstream)

      val putter: Reads[JsObject] = {
        (__).json.update(
          __.read[JsObject].map(root => JsObject(
            // Huh? Despite the meta being at the start, it always gets put at the end?!
            ("breadcrumbs", allMeta.as[JsValue]) +: root.as[JsObject].fields
          ))
        )
      }

      json.transform(putter).get


//      JsObject {
//        val withMeta = json.transform(putter).get
//
//        //temporary hack to put meta at the top
//        val fieldsAsMap: Map[String, JsValue] = withMeta.fields.toMap
//        val meta: JsValue = fieldsAsMap("meta")
//        ("meta", meta) +: fieldsAsMap.-("meta").toSeq
//      }
    }
  }

  object MetaBuilder{
    def init(json: JsValue): MetaBuilder = {
      // Assume that the JSON we are adding meta to has no existing meta
      assume(
        json.validate( (__ \ 'meta).json.pick ).isError,
        "can't add meta to a document that already contains meta"
      )
      MetaBuilder(JsObject.empty, json)
    }
  }
}