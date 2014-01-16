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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import org.junit.After
import java.nio.file.Paths
import java.nio.file.Path
import org.junit.Test
import java.io.FileNotFoundException
import java.nio.file.Files
import scala.io.Source
import java.nio.file.StandardOpenOption
import org.scalatest.Matchers

class CSVTest extends AssertionsForJUnit with Matchers {
  val dir = Paths.get("src", "test", "resources", "data")
  
  case class Row(i: Int, b: Boolean, s: String, d: Double)
  object Row{
	  val fieldNames = Seq("i", "b", "s", "d")
  }
  
  val tempFile = dir.resolve("CSVFileTest.csv")
  val existingTable = dir.resolve("testTable.csv")
  
  @Before
  def before {
  	if(Files.exists(tempFile)) Files.delete(tempFile)
  }
  
  @After
  def tearDown {
    Files.deleteIfExists(tempFile)
  }
  
  @Test def getHeaderNames {
  	val expected = Map("Ints" -> 0, "Booleans" -> 1, "Strings" -> 2, "Doubles" -> 3)
  	assertResult(expected)(CSV.header(existingTable))
  }
  
  @Test def readWithoutHeader {
  	val expected = Seq(
  		Seq("Ints", "Booleans", 	"Strings", 	"Doubles"),
  		Seq("1", 	"TRUE", 	"A",		"0.789"),
  		Seq("2",	"false", 	"ListOf", 	"0.2"),
  		Seq("3",	"false", 	"Strings", 	"0.64", 	"unexpected")
  	)
  	
  	assertResult(expected)(CSV.read(existingTable).map(_.toList).toList)
  }
 
  @Test def readByHeader {
    val expected = Seq(
  		Seq("Ints", "Doubles"),
  		Seq("1", 	"0.789"),
  		Seq("2",	"0.2"),
  		Seq("3",	"0.64")
  	)
  	
  	assertResult(expected){
  		CSV.readByHeader(existingTable, "Ints", "Doubles")
  			.map(_.toList)
  			.toList
    }
  }

  @Test def assertingHeader {
  	CSV.assertHeader(existingTable, "Ints", "Booleans", "Strings", "Doubles")
  }
  
  @Test def exceptionIfUnexpectedHeader {
    intercept[AssertionError]{
  		CSV.assertHeader(existingTable, "Ints", "Bolean", "Strings", "Doubles")
  	}
    intercept[AssertionError]{
  		CSV.assertHeader(existingTable, "Ints", "Boolean", "Strings")
  	}
  }
  
  @Test def writingSingleLine {
    val line1 = Seq("1", "Ayeeee", false)
    
  	CSV.writeLine(tempFile, line1)
  	
  	val lines = Source.fromFile(tempFile.toFile()).getLines.toIndexedSeq
  	
  	assert(lines(0) === "1,Ayeeee,false")
    assert(lines.size === 1)
  }
  
  @Test def writingMultipleLines {
    val line1 = Seq("1", "Ayeeee", false)
    val line2 = Seq("2", "Bee", true)
    
    CSV.writeLine(tempFile, line1)
    CSV.writeLine(tempFile, line2)		// First line is overwritten without the append option

    val lines = Source.fromFile(tempFile.toFile()).getLines.toIndexedSeq
    
  	assert(lines(0) === "2,Bee,true")
    assert(lines.size === 1)
  }
  
  @Test def appendingSingleLine {
  	val testData1 = Seq(
  		Seq(1,"Ayeeee",false),
  		Seq(2 ,"Bee" ,true)
  	)
  	
  	val testData2 = Seq(3,"Sea",false)

  	CSV.writeLines(tempFile, testData1)
  	CSV.writeLine(tempFile, testData2, StandardOpenOption.APPEND)

  	val lines = Source.fromFile(tempFile.toFile()).getLines.toIndexedSeq
  	
  	assert(lines(0) === "1,Ayeeee,false")
  	assert(lines(1) === "2,Bee,true")
  	assert(lines(2) === "3,Sea,false")
  	assert(lines.size === 3)
  }
  
  @Test def appendingMultipleLines {
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
  
  @Test def transposeFileWithHeader {
    val transposeTable = dir.resolve("transposeTable.csv")
    
  	CSV.transpose(transposeTable, tempFile, false)
  	
  	val lines = Source.fromFile(tempFile.toFile()).getLines.toIndexedSeq
  	
  	assert(lines(0) === "Number,1,3,5")
    assert(lines(1) === "Wang,2,4,6")
  }
  
  @Test def transposeFileDropHeader {
    val transposeTable = dir.resolve("transposeTable.csv")
			  
    CSV.transpose(transposeTable, tempFile, true)
			  
    val lines = Source.fromFile(tempFile.toFile()).getLines.toIndexedSeq
			  
    assert(lines(0) === "1,3,5")
    assert(lines(1) === "2,4,6")
  }
  
  @Test def overwriteWithFewerLinesRemovesAllPreviousLines {
    val d1 = Seq("1","2","3")
    val d2 = Seq("a","b","c")
      
    CSV.writeLines(tempFile, Seq(d1, d1, d2))
    CSV.writeLines(tempFile, Seq(d1, d2))

    val writtenLines = Source.fromFile(tempFile.toFile).getLines.toList
    
    assert(writtenLines(0) === "1,2,3")
    assert(writtenLines(1) === "a,b,c")
    assert(writtenLines.length === 2)
  }
  
  @Test def exceptionIfToStringOnObjectResultsInStringContainingComma {
  	fail("todo")	// TODO could be problematic - 
  }
}