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

package sampler.io.table

import java.nio.file.Path
import sampler.io.table.Types._
import scala.io.Source
import java.io.FileWriter
import java.nio.file.Files
import java.nio.file.FileAlreadyExistsException
import scala.Array.canBuildFrom

trait TableWriter{
	def apply(columns: Column[_]*): Unit
}

class CSVTableWriter(path: Path, overwrite: Boolean = false) extends TableWriter{
	def apply(columns: Column[_]*){
		
		if(overwrite == false && Files.exists(path)) {
			throw new FileAlreadyExistsException(path.toString() + "exists and overwriting is not permitted")
		}
		
		columns.map {
			case a => columns.map {
				case b if (columns.indexOf(a) != columns.indexOf(b)) => if(a.name == b.name) throw new TableWriterException("Two columns have been supplied with the same name")
				case _ =>
			}
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
