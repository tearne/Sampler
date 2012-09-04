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
import sampler.math.Probability

@RunWith(classOf[JUnitRunner])
class CSVTableWriterSpec extends Specification{

	val path = Paths.get(new File("").getAbsolutePath())
	val filePath = path.resolve("testFile.csv")
	val file = Files.createFile(filePath)
	
	"CSVTableWriter" should{

		"throw an exception" in {

			"when no data is supplied to the apply method" in new fileSetup with fileTearDown {
				writer.apply() must throwA[TableWriterException]
			}
			
			"when the header name is None" in new fileSetup with fileTearDown {
				val params1 = Seq(1,2,3)
				val params2 = Seq(3.0,2.0,1.0)
						
				val tc1 = new Column(params1)
				val tc2 = new Column(params2, Some("Parameter2"))
				
				writer.apply(tc1, tc2) must throwA[TableWriterException]
			}
			
			"columns are of different lengths" in new fileSetup with fileTearDown {
				val params1 = Seq(1,2,3)
				val params2 = "Lots and lots and lots of entries".split(" ")
						
				val col1 = new Column(params1, Some("SomeInts"))
				val col2 = new Column(params2, Some("LoadsaStrings"))
				
				writer.apply(col1, col2) must throwA[TableWriterException]
			}
		}
		
		"create a file" in new fileSetup with fileTearDown {
			val params = Seq(1,2,3)
			val col = new Column(params, Some("MyInts"))
			
			writer.apply(col)
			Files.exists(file) == true
		}
		
		"write data to a file" in {
			
			"when the data type is ints" in new fileSetup with fileTearDown {
				val ints = Seq(1,2)
				val intCol = new Column(ints, Some("MyInts"))
				
				writer.apply(intCol)
				val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
				
				val expectedLines = Array("MyInts", "1", "2") 
				
				lines mustEqual expectedLines
			}
			
			"when the data type is doubles" in new fileSetup with fileTearDown {
				val doubles = Seq(2.000,1.0)
				val doubleCol = new Column(doubles, Some("MyDoubles"))
				
				writer.apply(doubleCol)
				val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
				
				val expectedLines = Array("MyDoubles", "2.0", "1.0")
				
				lines mustEqual expectedLines
			}
			
			"when the data type is strings" in new fileSetup with fileTearDown {
				val strings = Seq("String", "List")
				val stringCol = new Column(strings, Some("MyStrings"))
				
				writer.apply(stringCol)
				val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
				
				val expectedLines = Array("MyStrings", "String", "List")
				
				lines mustEqual expectedLines
			}
			
			"when the data type is factors" in new fileSetup with fileTearDown {
				val factors = Seq(Factor("F1"), Factor("F2"))
				val factorCol = new Column(factors, Some("MyFactors"))
				
				writer.apply(factorCol)
				val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
				
				val expectedLines = Array("MyFactors", "F1", "F2")
				
				lines mustEqual expectedLines
			}
			
			"when the data type is booleans" in new fileSetup with fileTearDown {
				val booleans = Seq(true, false)
				val booleanCol = new Column(booleans, Some("MyBools"))
				
				writer.apply(booleanCol)
				val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
				
				val expectedLines = Array("MyBools", "true", "false")
				
				lines mustEqual expectedLines
			}
			
			"when the data type is probabilities" in new fileSetup with fileTearDown {
				val booleans = Seq(true, false)
				val booleanCol = new Column(booleans, Some("MyBools"))
				
				writer.apply(booleanCol)
				val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
				
				val expectedLines = Array("MyBools", "true", "false")
				
				lines mustEqual expectedLines
			}
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