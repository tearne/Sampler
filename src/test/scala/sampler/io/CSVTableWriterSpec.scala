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
import sampler.data.Types._
import org.specs2.specification.Scope
import java.nio.file.Files
import org.specs2.mutable.Before
import org.specs2.mutable.After
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class CSVTableWriterSpec extends Specification{

	val path = Paths.get(new File("").getAbsolutePath())
	val filePath = path.resolve("testFile.csv")
	val file = Files.createFile(filePath)
	
	"CSVTableWriter" should{
		"throw exception on unparsable values" in new fileSetup with fileTearDown {
			todo
		}

		"throw exception when no data supplied" in new fileSetup with fileTearDown {
			writer.apply() must throwA[TableWriterException]
		}
		
		"throw exception if header name is None" in new fileSetup with fileTearDown {
			val params1 = IndexedSeq(1,2,3)
			val params2 = IndexedSeq(3.0,2.0,1.0)
			
			val tc1 = new Column(params1)
			val tc2 = new Column(params2, Some("Parameter2"))
			
			writer.apply(tc1, tc2) must throwA[TableWriterException]
		}
		
		"throw exception if columns of different lengths" in new fileSetup with fileTearDown {
			val params1 = IndexedSeq(1,2,3)
			val params2 = IndexedSeq("Lots", "and", "Lots", "and", "Lots", "of", "Entries")
			
			val col1 = new Column(params1, Some("SomeInts"))
			val col2 = new Column(params2, Some("LoadsaStrings"))

			writer.apply(col1, col2) must throwA[TableWriterException]
		}
		
		"create a file" in new fileSetup with fileTearDown {
			val params = IndexedSeq(1,2,3)
			val col = new Column(params, Some("MyInts"))
			
			writer.apply(col)
			Files.exists(file) == true
		}
		
		"write ints" in new fileSetup with fileTearDown {
			val ints = IndexedSeq(1,2)
			val intCol = new Column(ints, Some("MyInts"))
			
			writer.apply(intCol)
			
			val expectedLine1 = "MyInts"
			val expectedLine2 = "1"
			val expectedLine3 = "2"
				
			val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
			val expectedLines = Array(expectedLine1, expectedLine2, expectedLine3)
			
			lines mustEqual expectedLines
		}
		
		"write doubles" in new fileSetup with fileTearDown {
			val doubles = IndexedSeq(2.000,1.0)
			val doubleCol = new Column(doubles, Some("MyDoubles"))
			
			writer.apply(doubleCol)
			
			val expectedLine1 = "MyDoubles"
			val expectedLine2 = "2.0"
			val expectedLine3 = "1.0"
			
			val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
			val expectedLines = Array(expectedLine1, expectedLine2, expectedLine3)
			
			lines mustEqual expectedLines
		}
		
		"write strings" in new fileSetup with fileTearDown {
			val strings = IndexedSeq("String", "List")
			val stringCol = new Column(strings, Some("MyStrings"))
			
			writer.apply(stringCol)
			
			val expectedLine1 = "MyStrings"
			val expectedLine2 = "String"
			val expectedLine3 = "List"
			
			val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
			val expectedLines = Array(expectedLine1, expectedLine2, expectedLine3)
			
			lines mustEqual expectedLines
		}
		
		"write factors" in new fileSetup with fileTearDown {
			val factors = IndexedSeq(Factor("F1"), Factor("F2"))
			val factorCol = new Column(factors, Some("MyFactors"))
			
			writer.apply(factorCol)
			
			val expectedLine1 = "MyFactors"
			val expectedLine2 = "F1"
			val expectedLine3 = "F2"
			
			val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
			val expectedLines = Array(expectedLine1, expectedLine2, expectedLine3)
			
			lines mustEqual expectedLines
		}
		
		"write booleans" in new fileSetup with fileTearDown {
			val booleans = IndexedSeq(true, false)
			val booleanCol = new Column(booleans, Some("MyBools"))
			
			writer.apply(booleanCol)
			
			val expectedLine1 = "MyBools"
			val expectedLine2 = "true"
			val expectedLine3 = "false"
			
			val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
			val expectedLines = Array(expectedLine1, expectedLine2, expectedLine3)
			
			lines mustEqual expectedLines
		}
		
		"write probabilities with their containing value" in new fileSetup with fileTearDown {
			val booleans = IndexedSeq(true, false)
			val booleanCol = new Column(booleans, Some("MyBools"))
			
			writer.apply(booleanCol)
			
			val expectedLine1 = "MyBools"
			val expectedLine2 = "true"
			val expectedLine3 = "false"
			
			val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
			val expectedLines = Array(expectedLine1, expectedLine2, expectedLine3)
			
			lines mustEqual expectedLines
		}
	}

	trait fileSetup extends Scope {
		val writer = new CSVTableWriter(filePath)
	}
	
	trait fileTearDown extends After {
		def after = Files.deleteIfExists(file)
//		def after = {}
	}
}