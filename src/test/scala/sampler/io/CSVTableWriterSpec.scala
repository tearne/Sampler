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

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import java.nio.file.Paths
import java.io.File
import sampler.data.TableColumn
import org.specs2.specification.Scope
import java.nio.file.Files
import org.specs2.mutable.Before
import org.specs2.mutable.After
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class CSVTableWriterSpec extends Specification{
	"CSVTableWriter" should{
		"throw an exception" in {
			"supplied file path does not point to a csv file" in {
				val path = Paths.get(new File("").getAbsolutePath())
				val filePath = path.resolve("invalidFile.notacsv")
				
				new CSVTableWriter(filePath) must throwA[TableWriterException]	
			}
			"unparsable values" in todo
			"header not found in table columns" in {
				val path = Paths.get(new File("").getAbsolutePath())
				val filePath = path.resolve("testFile.csv")
				
				val writer = new CSVTableWriter(filePath)
				
				val params1 = IndexedSeq(1,2,3)
				val params2 = IndexedSeq(3.0,2.0,1.0)
				
				val tc1 = new TableColumn(params1)
				val tc2 = new TableColumn(params2, Some("Parameter2"))
				
				writer.apply(tc1, tc2) must throwA[TableWriterException]
			}
		} 
	}
	
	"CSVTableWriter" should {
		"create the file" in new fileSetup with fileTearDown {
			writer.apply(tc1, tc2, tc3)
			Files.exists(file) == true
		}
		"write the four lines containing the headers and data points" in new fileSetup with fileTearDown {
			writer.apply(tc1, tc2, tc3)
			
//			read in file and check correct contents
			val expectedLine1 = "P1,P2,P3"
			val expectedLine2 = "1,3.0,one"
			val expectedLine3 = "2,2.0,two"
			val expectedLine4 = "3,1.0,three"
				
			val source = Source.fromFile(filePath.toString())
			val sourceString = source.mkString
			
			val lines = sourceString.split("\n")
			val expectedLines = Array(expectedLine1, expectedLine2, expectedLine3, expectedLine4)
			
			lines mustEqual expectedLines
		}
	}
	
	val path = Paths.get(new File("").getAbsolutePath())
	val filePath = path.resolve("testFile.csv")
	val file = Files.createFile(filePath)
			
	trait fileSetup extends Scope {
		val writer = new CSVTableWriter(filePath)
				
		val params1 = IndexedSeq(1,2,3)
		val params2 = IndexedSeq(3.0,2.0,1.0)
		val params3 = IndexedSeq("one", "two", "three")
				
		val tc1 = new TableColumn(params1, Some("P1"))
		val tc2 = new TableColumn(params2, Some("P2"))
		val tc3 = new TableColumn(params3, Some("P3"))
	}
	
	trait fileTearDown extends After {
		def after = Files.deleteIfExists(file)
//		def after = {}
	}
}