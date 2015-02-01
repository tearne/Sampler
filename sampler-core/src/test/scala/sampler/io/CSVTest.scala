/*
 * Copyright (c) 2013 Crown Copyright 
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

import java.nio.file.Paths
import java.nio.file.Path
import java.io.FileNotFoundException
import java.nio.file.Files
import scala.io.Source
import java.nio.file.StandardOpenOption
import sampler.math.Partition
import org.scalatest.FreeSpec
import org.scalatest.BeforeAndAfter

class CSVTest extends FreeSpec with BeforeAndAfter {
  val dir = Paths.get(getClass.getClassLoader.getResource("data").toURI())
  
  case class Row(i: Int, b: Boolean, s: String, d: Double)
  object Row{
	  val fieldNames = Seq("i", "b", "s", "d")
  }
  
  val tempFile = dir.resolve("CSVFileTest.csv")
  val existingTable = dir.resolve("testTable.csv")
  
  before {
  	if(Files.exists(tempFile)) Files.delete(tempFile)
  }
  
  after {
    Files.deleteIfExists(tempFile)
  }
  
  "Get header names" in {
  	val expected = Map("Ints" -> 0, "Booleans" -> 1, "Strings" -> 2, "Doubles" -> 3)
  	assertResult(expected)(CSV.header(existingTable))
  }
  
  "Read without header" in {
  	val expected = Seq(
  		Seq("Ints", "Booleans", 	"Strings", 	"Doubles"),
  		Seq("1", 	"TRUE", 	"A",		"0.789"),
  		Seq("2",	"false", 	"ListOf", 	"0.2"),
  		Seq("3",	"false", 	"Strings", 	"0.64", 	"unexpected")
  	)
  	
  	assertResult(expected)(CSV.read(existingTable).map(_.toList).toList)
  }

  "Asert header" in {
  	CSV.assertHeader(existingTable, "Ints", "Booleans", "Strings", "Doubles")
  }
  
  "Exception if unexpected header" in {
    intercept[AssertionError]{
  		CSV.assertHeader(existingTable, "Ints", "Bolean", "Strings", "Doubles")
  	}
    intercept[AssertionError]{
  		CSV.assertHeader(existingTable, "Ints", "Boolean", "Strings")
  	}
  }
  
  "Writing single line" in {
    val line1 = Seq("1", "Ayeeee", false)
    
  	CSV.writeLine(tempFile, line1)
  	
  	val lines = Source.fromFile(tempFile.toFile()).getLines.toIndexedSeq
  	
  	assert(lines(0) === "1,Ayeeee,false")
    assert(lines.size === 1)
  }
  
  "Writing multiple lines" in {
    val line1 = Seq("1", "Ayeeee", false)
    val line2 = Seq("2", "Bee", true)
    
    CSV.writeLine(tempFile, line1)
    CSV.writeLine(tempFile, line2)		// First line is overwritten without the append option

    val lines = Source.fromFile(tempFile.toFile()).getLines.toIndexedSeq
    
  	assert(lines(0) === "2,Bee,true")
    assert(lines.size === 1)
  }
  
  "Appending single line" in {
  	val testData1 = Seq(
  		Seq(1,"Ayeeee",false),
  		Seq(2 ,"Bee" ,true)
  	)
  	
  	val testData2 = Seq(
  	    Seq(3,"Sea",false)
  		)
  	    
  	CSV.writeLines(tempFile, testData1)
  	CSV.writeLines(tempFile, testData2, StandardOpenOption.APPEND)

  	val lines = Source.fromFile(tempFile.toFile()).getLines.toIndexedSeq
  	
  	assert(lines(0) === "1,Ayeeee,false")
  	assert(lines(1) === "2,Bee,true")
  	assert(lines(2) === "3,Sea,false")
  	assert(lines.size === 3)
  }
  
  "Appending multiple lines" in {
  	val testData1 = Seq(
  		Seq(1,"Ayeeee",false),
  		Seq(2 ,"Bee" ,true)
  	)
  	
  	val testData2 = Seq(
  		Seq(3,"Sea",false),
  		Seq(4,"Dea",false)
  	)

  	CSV.writeLines(tempFile, testData1)
  	CSV.writeLines(tempFile, testData2, StandardOpenOption.APPEND)

  	val lines = Source.fromFile(tempFile.toFile()).getLines.toIndexedSeq
  	
  	assert(lines(0) === "1,Ayeeee,false")
  	assert(lines(1) === "2,Bee,true")
  	assert(lines(2) === "3,Sea,false")
  	assert(lines(3) === "4,Dea,false")
  	assert(lines.size === 4)
  }
  
  "Transpose" in {
    val transposeTable = dir.resolve("transposeTable.csv")
    
  	CSV.transpose(transposeTable, tempFile)
  	
  	val lines = Source.fromFile(tempFile.toFile()).getLines.toIndexedSeq
  	
  	assert(lines(0) === "Number,1,3,5")
    assert(lines(1) === "Wang,2,4,6")
  }
  
  "Overwrite with fewer lines removes all previous lines" in {
    val d1 = Seq("1","2","3")
    val d2 = Seq("a","b","c")
      
    CSV.writeLines(tempFile, Seq(d1, d1, d2))
    CSV.writeLines(tempFile, Seq(d1, d2))

    val writtenLines = Source.fromFile(tempFile.toFile).getLines.toList
    
    assert(writtenLines(0) === "1,2,3")
    assert(writtenLines(1) === "a,b,c")
    assert(writtenLines.length === 2)
  }
  
  "Exception if toString on object results in string containing comma" in {
  	val p1 = Partition(IndexedSeq(0.1, 0.9))		// Partitions should print with a comma
  	val p2 = Partition(IndexedSeq(0.5, 0.5))
  	
  	val data = Seq(
  			Seq("Head1", "Head2"),
  			Seq(p1, p2)
  	)
  	
  	intercept[AssertionError] {
  	  CSV.writeLines(tempFile, data)
  	}
  }
}