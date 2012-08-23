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

trait TableReader{
	def get[T](params: TableHeader[T]): IndexedSeq[T]
}
trait TableWriter{
	def apply(columns: TableColumn[_]*): Unit
}

class CSVTableWriter(path: Path, overwrite: Boolean = false, append: Boolean = false) extends TableWriter{
	
	if(path.toString.endsWith(".csv") == false) {
		throw new TableWriterException("Invalid csv file name supplied to csv writer")
	}
	
	def apply(columns: TableColumn[_]*){
		columns.map{_ match {
			case a if(a.name.getOrElse(false) == false) => throw new 
					TableWriterException("each column must have name for the column header")
			case _ =>
		}}
		
		val csvFile = new FileOutputStream(path.toString)
		val csvStream = new PrintStream(csvFile)
		
		for(i <- 0 until columns.length) {
			csvStream.print(columns(i).name.get)
			if(i < columns.length-1)
				csvStream.print(",")
		}

		csvStream.print("\n")
		
		var mainSeq : Seq[Seq[Any]] = Seq()
		
		columns.map{_ match {
			case a => mainSeq = mainSeq :+ a.values
		}}
		
		mainSeq=mainSeq.transpose
		
		mainSeq.map{_ match{
			case a => {for(i <- 0 until a.length) {
				csvStream.print(a(i)) 
				if(i< a.length-1) {csvStream.print(",")}}	
				csvStream.print("\n")
			}
		}}
		
		csvStream.close()
	}
}

class TableWriterException(msg: String) extends RuntimeException(msg)
