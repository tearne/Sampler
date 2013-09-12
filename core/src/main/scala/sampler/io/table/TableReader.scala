/*
 * Copyright (c) 2012-13 Crown Copyright 
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

trait TableReader{
	def get[T](params: Header[T]): Column[T]
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