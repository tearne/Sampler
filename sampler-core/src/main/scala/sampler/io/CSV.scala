package sampler.io

import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.OpenOption
import java.nio.file.Path
import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom
import scala.io.Source

object CSV {
	def header(filePath: Path): Map[String, Int] = {
		val strings = Source.fromFile(filePath.toFile).getLines.next.split(',').map(_.trim)
		strings.zipWithIndex.map{case (name, idx) => name -> idx}.toMap
	}
	
	def assertHeader(filePath: Path, expected:String*) {
		val strings = Source.fromFile(filePath.toFile).getLines.next.split(',').map(_.trim).toSeq
		assert(strings == expected, {
			val newLine = System.getProperty("line.separator") 
s"""Headers in ${filePath.toAbsolutePath()} don't match. 
	expected: 	${expected.toString}
	found:		${strings.toString}
"""})
	}
	
	def writeLine(filePath: Path, line: Traversable[Any], openOptions: OpenOption*) {
		val writer = getWriter(filePath, openOptions: _*)
		writer.write(toStrings(line).mkString(","))
		writer.newLine()
		writer.close()
	}
	
	//TODO option to check that the file (including any existing data?) isn't ragged?
	def writeLines(filePath: Path, lines: Traversable[Traversable[Any]], openOptions: OpenOption*) {
		val writer = getWriter(filePath, openOptions: _*)
		lines.foreach{line => 
			writer.write(toStrings(line).mkString(","))
			writer.newLine()
		}
		writer.close()
	}
	
	private def toStrings(line: Traversable[Any]): Traversable[String] = {
		val strings = line.map(_.toString)
		assert(!strings.exists(_.contains(',')), "CSV lines may not contain commas: "+strings.mkString(" | "," | "," | "))
		strings
	}
	
	def transpose(inPath: Path, outFile: Path, outputOpenOptions: OpenOption*) {
		val matrix = Source.fromFile(inPath.toFile).getLines.map(line => line.split(',').map(_.trim).toList).toList
		writeLines(outFile, matrix.transpose, outputOpenOptions: _*)
	}
	
	def read(filePath: Path): Iterator[IndexedSeq[String]] = {
		Source.fromFile(filePath.toFile()).getLines.map{line =>
			line.split(',').map(_.trim)
		}
	}
	
	def readByHeader(filePath: Path, headers: String*): Iterator[IndexedSeq[String]] = {
		val tokenisedLines = read(filePath)
		val headerMap = header(filePath)
		val headerIndexes = headers.map(headerMap).toArray
		
		tokenisedLines.map{lineToks =>
			headerIndexes.map(lineToks)
		}
	}
	
	private def getWriter(filePath: Path, openOptions: OpenOption*) = {
		Files.newBufferedWriter(
				filePath, 
				Charset.defaultCharset(), 
				openOptions:_*
		)
	}
}