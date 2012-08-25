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

trait TableReader{
	def get[T](params: Header[T]): Column[T]
}
trait TableWriter{
	def apply(columns: Column[_]*): Unit
}

class CSVTableReader(path: Path) extends TableReader{
	
	val source = Source.fromFile(path.toString())
			.getLines()
			.map(_.split(",").map(_.trim))
			.toIterable
	
	def get[T](header: Header[T]): Column[T] = {
		val it = source.iterator
		val headers = it.next()
		
		val columnIdx = headers.indexWhere(_ == header.name) match{
			case i: Int if i<0 => throw new UnsupportedOperationException("Header not found, TODO: improve exception")
			case i => i 
		}
		
		val values = it.map(row => header.cType(row(columnIdx))).toIndexedSeq
		new Column(values, Option(header.name))(header.cType)
	}
}

class CSVTableWriter(path: Path, overwrite: Boolean = false, append: Boolean = false) extends TableWriter{
	
	if(path.toString.endsWith(".csv") == false) {
		throw new TableWriterException("Invalid csv file name supplied to csv writer")
	}
	
	def apply(columns: Column[_]*){
		columns.map{
			case a if(a.name.getOrElse(false) == false) => throw new 
					TableWriterException("each column must have name for the column header")
			case _ =>
		}
		
		def makeCSVLine(tokens: Iterable[Any]) = {
			val newLine: String = System.getProperty("line.separator")
			val it = tokens.iterator
			val builder = new StringBuilder()
			it.foreach(dat => {
				builder.append(dat)
				if(it.hasNext)
					builder.append(",")
			})
			builder.append(newLine)
			builder.toString
		}
		
		val csvFile = new FileOutputStream(path.toString)
		val csvStream = new PrintStream(csvFile)
		
		var names: Seq[Any] = Seq()
		var mainSeq : Seq[Seq[Any]] = Seq()
		
		columns.map(_.toStringColumn).map{
			case a => {
				names = names :+ a.name.get
				mainSeq = mainSeq :+ a.values
			}
		}

		csvStream.print(makeCSVLine(names))
		
		mainSeq=mainSeq.transpose
		
		mainSeq.map{
			case a => csvStream.print(makeCSVLine(a))
		}
		
		csvStream.close()
	}
}

class TableWriterException(msg: String) extends RuntimeException(msg)
class ReaderFileException(msg: String) extends RuntimeException(msg)
