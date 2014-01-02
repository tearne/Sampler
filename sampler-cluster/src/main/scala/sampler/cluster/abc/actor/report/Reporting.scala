package sampler.cluster.abc.actor.report

import java.nio.file.Path
import sampler.cluster.abc.actor.Report
import sampler.io.CSV
import scala.io.Source
import java.nio.file.Files
import java.nio.charset.Charset
import java.nio.file.StandardOpenOption

//TODO integrate with the rest of the CSV stuff?
trait Writable{
	def fieldNames: Seq[String]
	def fields: Seq[String]
}