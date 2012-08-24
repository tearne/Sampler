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
import sampler.data.TableHeader
import sampler.data.TableColumn
import java.io.FileOutputStream
import java.io.PrintStream
import scala.io.Source
import java.io.FileNotFoundException

trait TableReader{
	def get[T](params: TableHeader[T]): IndexedSeq[T]
}
trait TableWriter{
	def apply(columns: TableColumn[_]*): Unit
}

class CSVTableReader(path: Path) extends TableReader{
	
	var source: Iterable[Array[String]] = _
	
	try {
		source = Source.fromFile(path.toString()).getLines().map(_.split(",").map(_.trim)).toIterable
	} catch {
		case e: FileNotFoundException => throw new ReaderFileException(path.toString() + " does not point to a file")
		case _ =>
	}
	
	def get[T](params: TableHeader[T]): IndexedSeq[T] = {
		
		val it = source.iterator
		
		val headers = it.next()
		
		headers.map{
			case a if (params.name == a) => println(headers.indexOf(a))
			case _ =>
		}
		
		null
	}
}

class CSVTableWriter(path: Path, overwrite: Boolean = false, append: Boolean = false) extends TableWriter{
	
	if(path.toString.endsWith(".csv") == false) {
		throw new TableWriterException("Invalid csv file name supplied to csv writer")
	}
	
	def apply(columns: TableColumn[_]*){
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
		
		columns.map{
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
