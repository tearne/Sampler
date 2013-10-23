/*
 * Copyright (c) 2013 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.io

import java.nio.file.{Path, Files, StandardOpenOption}
import java.nio.charset.Charset
import scala.io.Source
import java.nio.file.OpenOption
import java.nio.file.StandardOpenOption._

object CSVFile {
	lazy val newLine = System.getProperty("line.separator")
		
	def header(filePath: Path): IndexedSeq[String] = {
		Source.fromFile(filePath.toFile).getLines.next.split(',').map(_.trim)
	}
	
	// TODO read only one column from header
	def read[T](filePath: Path, parser: String => T, header: Seq[String]): Iterator[T] = 
		read(filePath, parser, Some(header))
	def read[T](filePath: Path, parser: String => T, header: Option[Seq[String]]): Iterator[T] = {
		val lines = Source.fromFile(filePath.toFile()).getLines
		
		if(header.isDefined) checkHeader(header.get, lines.next)
		
		lines.map(l => parser(l))
	}
	
	def write(filePath: Path, data: Traversable[String], header: Seq[String], options: OpenOption*) {
		assert(!(options.contains(APPEND) && options.contains(TRUNCATE_EXISTING)), "Can't both append and overwrite")
		
		val lines = if(options.contains(APPEND)){
			if(!header.isEmpty) {
				checkHeader(header, Source.fromFile(filePath.toFile()).getLines.next)
			}
			data			
		} else if(options.contains(TRUNCATE_EXISTING)){
			Iterator(header.reduce(_ + "," + _)) ++ data
		} else {
			if(header.isEmpty) data
			else Iterator(header.reduce(_ + "," + _)) ++ data
		}
		
		val writer = Files.newBufferedWriter(
				filePath, 
				Charset.defaultCharset(), 
				options:_*
		)
		
		lines.foreach{line =>
			assert(header.size == line.split(',').size, "Encountered line of different length to header: "+line)
			writer.append(line)
			writer.newLine()
		}
		writer.close
		
	}
	
	private def checkHeader(expected: Seq[String], actual: String) = {
		val expectedTrim = expected.map(_.trim)
		val actualTrim = actual.split(',').map(_.trim)
		assert(
			actualTrim sameElements expected,
			s"Headers don't match. ${newLine}Expected $expected${newLine}     Got ${actualTrim}"
		)
	}
}