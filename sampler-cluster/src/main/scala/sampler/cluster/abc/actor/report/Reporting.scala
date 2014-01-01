package sampler.cluster.abc.actor.report

import java.nio.file.Path
import sampler.cluster.abc.actor.Report
import sampler.io.CSVFile
import scala.io.Source
import java.nio.file.Files
import java.nio.charset.Charset
import java.nio.file.StandardOpenOption

//TODO integrate with the rest of the CSV stuff?
trait Writable{
	def fieldNames: Seq[String]
	def fields: Seq[String]
}

object Reporting {
	def individualFiles[P <: Writable](directory: Path, fileNamePrefix: String): Report[P] => Unit = {
		report => {
			import report._
			CSVFile.write(
					directory.resolve(s"$fileNamePrefix.$generationId.csv"), 
					posterior.toTraversable.map(_.fields.mkString), 
					posterior.head.fieldNames
			)
		}
	}
}