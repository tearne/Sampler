/*
 * Copyright (c) 2012 Crown Copyright 
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

import java.nio.file.Path
import sampler.data.Types._
import java.io.FileOutputStream
import java.io.PrintStream
import scala.io.Source
import java.io.FileNotFoundException
import scala.reflect.Manifest
import scala.collection.immutable.WrappedString
import java.io.FileWriter
import scala.util.matching.Regex
import java.io.File
import java.nio.file.Files
import java.nio.file.FileAlreadyExistsException

trait TableReader{
	def get[T](params: Header[T]): Column[T]
}
trait TableWriter{
	def apply(columns: Column[_]*): Unit
}

class CSVTableReader(path: Path) extends TableReader{
	val source = Source.fromFile(path.toString())
					.getLines()
					.map(_.replace("\"",""))
					.map(_.split(",").map(_.trim))
					.toIterable
	
	def get[T](header: Header[T]): Column[T] = {
		val it = source.iterator
		val headers = it.next()
		
		val columnIdx = headers.indexWhere(_ == header.name) match{
			case i: Int if i<0 => throw new UnsupportedOperationException("Header not found, TODO: improve exception")
			case i => i 
		}
		
		try {
			val values = it.map(row => header.cType(row(columnIdx))).toIndexedSeq
			new Column(values, header.name)(header.cType)
		} catch {
			case nfe: NumberFormatException => throw new TableReaderException("Could not parse data for column " + header.name)
		}
	}
}

class CSVTableWriter(path: Path, overwrite: Boolean = false) extends TableWriter{
	def apply(columns: Column[_]*){
		
		if(overwrite == false && Files.exists(path)) {
			throw new FileAlreadyExistsException(path.toString() + "exists and overwriting is not permitted")
		}
		
		try {
			val colLength1 = columns(0).values.length
			
			columns.foreach {
				case a  if(a.values.length == colLength1) =>
				case _ => throw new TableWriterException("attempted to write a table where columns not of equal length")
			}
		} catch {
			case ioobe: IndexOutOfBoundsException => throw new TableWriterException("Tried to write a table with no data")
		}
		
		def makeCSVLine(tokens: Iterable[String]) = {
			val newLine = System.getProperty("line.separator")
			val it = tokens.iterator
			val builder = new StringBuilder()
			it.foreach(value => {
				builder.append(value)
				if(it.hasNext) builder.append(",")
			})
			builder.append(newLine)
			builder.toString
		}
		
		val writer = new FileWriter(path.toFile)
		
		val headerLine = makeCSVLine(columns.map(col => 
			col.name
		))
		
		writer.append(headerLine)
		
		columns.map(col => col.toStringColumn.values).transpose.foreach{row =>
			writer.append(makeCSVLine(row))
		}
		
		writer.close
	}
}

class TableWriterException(msg: String) extends RuntimeException(msg)
class TableReaderException(msg: String) extends RuntimeException(msg)
