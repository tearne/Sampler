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

object CSVFile {
	lazy val newLine = System.getProperty("line.separator")
		
	// TODO read only one column from header
	def read[T](file: Path, parser: String => T, header: Seq[String]): Iterator[T] = read(file, parser, Some(header))	
	def read[T](file: Path, parser: String => T, header: Option[Seq[String]]): Iterator[T] = {
		val lines = Source.fromFile(file.toFile()).getLines
		
		if(header.isDefined) checkHeader(header.get, lines.next)
		
		lines.map(l => parser(l))
	}
	
	// TODO change append and overwrite 
	// TODO Bug when overwriting with fewer lines
	def write(file: Path, data: Traversable[String], append: Boolean = false, overwrite: Boolean = false, header: Seq[String] = Seq.empty) {
		assert(!(append && overwrite), "Can't both append and overwrite")
		
		val lines = if(append){
			if(!header.isEmpty) {
				checkHeader(header, Source.fromFile(file.toFile()).getLines.next)
			}
			data			
		} else if(overwrite){
			Iterator(header.reduce(_ + "," + _)) ++ data
		} else {
			assert(!Files.exists(file), "File exists but no overwrite or append option set")
			if(header.isEmpty) data
			else Iterator(header.reduce(_ + "," + _)) ++ data
		}
		
		
//		if(append && !header.isEmpty) checkHeader(header, Source.fromFile(file.toFile()).getLines.next)
//		val toWrite = 
//			if(append) data
//			else if(!header.isEmpty) Iterator(header.reduce(_ + "," + _)) ++ data
//			else data
			
		val writer = Files.newBufferedWriter(
				file, 
				Charset.defaultCharset(), 
				if(append) StandardOpenOption.APPEND else StandardOpenOption.CREATE
		)
		lines.foreach{line =>
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