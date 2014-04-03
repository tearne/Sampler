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

package sampler.columnbasedtable

import java.nio.file.Path
import scala.io.Source
import java.io.FileWriter
import java.nio.file.Files
import java.nio.file.FileAlreadyExistsException
import scala.Array.canBuildFrom

//TODO simplify - remove trait

/** Trait for objects which can read data from files */
trait TableReader{
  /** Creates a new [[sampler.io.table.Column]] of type T containing the data
   *  associated with the requested [[sampler.io.table.Header]] 
   *  
   *  @param params Header determining which section of the target file is to be read in
   *  @return Column containing the data
   */
  def get[T](params: Header[T]): Column[T]
}

/** Implementation of [[sampler.io.table.TableReader]] for reading in data from .csv files
 *  
 *  @constructor Create a new CSVTableReader to read in the file given by the path parameter
 *  @param path Path pointing to the file of interest
 */
class CSVTableReader(path: Path) extends TableReader{
	private val source = Source.fromFile(path.toString())
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