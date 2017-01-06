package sampler.example.abc.flockMortality.util

import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path

import play.api.libs.json.JsValue
import play.api.libs.json.Json

object JSON {
  
  // Read json file as string
  def readJSON(path: Path): JsValue = {
    val br = Files.newBufferedReader(path, Charset.defaultCharset())
    val string = Stream.continually(br.readLine()).takeWhile(_ != null).mkString("\n")
    Json.parse(string)
  }
  
  
  def writeToFile(path: Path, json: JsValue) = {
    val writer = Files.newBufferedWriter(path, Charset.defaultCharset())
    writer.write(Json.prettyPrint(json))
    writer.close()
  }

}