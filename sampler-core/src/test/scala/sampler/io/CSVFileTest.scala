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
import org.scalatest.matchers.ShouldMatchers
import org.junit.Before
import org.junit.After
import java.nio.file.Paths
import java.nio.file.Path
import org.junit.Test
import java.io.FileNotFoundException
import sampler.math.Probability
import java.nio.file.Files
import scala.io.Source
import java.nio.file.StandardOpenOption

class CSVFileTest extends AssertionsForJUnit with ShouldMatchers {
  val dir = Paths.get("src", "test", "resources", "data")
  
  case class Row(i: Int, b: Boolean, s: String, p: Probability, d: Double)
  object Row{
  	def parser(line: String) = {
  		val toks = line.split(',').map(_.trim)
  		Row(
  				toks(0).toInt,
  				toks(1).toBoolean,
  				toks(2),
  				Probability(toks(3).toDouble),
  				toks(4).toDouble
  		)
  	}
  }
  
  val file = dir.resolve("CSVFileTest.csv")
  
  @Before
  def before {
  	if(Files.exists(file)) Files.delete(file)
  }
  
  @After
  def tearDown {
    Files.deleteIfExists(file)
  }
  
  @Test def readWithHeader {
    val data = CSVFile
    		.read(dir.resolve("testTable.csv"), Row.parser _, Seq("MyInts", "TheirBools", "Strings", "Probs", "Doubles"))
    		.toIndexedSeq
	assert(data(0) === Row(1, true, "A", Probability(0.789), 1.0))
    assert(data(1) === Row(2, false, "ListOf", Probability(0.2), 0.888))
    assert(data(2) === Row(3, false, "Strings", Probability(0.64), 3.0))
  }
  
  @Test def exceptionIfReadHeadersDontMatch{
  	intercept[AssertionError]{CSVFile
    		.read(dir.resolve("testTable.csv"), Row.parser _, Seq("MyInts", "TheirBools", "Strings", "Proooobs", "Doubles"))
    		.toIndexedSeq
  	}
  }
  
  @Test def writingAndAppendingWithHeader {
  	val testHeader = Seq("Int", "String", "Boolean")
  	val testData1 = 
"""1,Ayeeee,False
2 ,Bee ,True
"""
  	
  	val testData2 = 
"""3,Sea,False
4 ,Dea ,False
"""
  	try{
  		CSVFile.write(file, testData1.lines.toIterable, testHeader)
  		CSVFile.write(file, testData2.lines.toIterable, testHeader, StandardOpenOption.APPEND)
  	}
  	
  	val lines = Source.fromFile(file.toFile()).getLines.toIndexedSeq
  	
  	assert(lines(0) === "Int,String,Boolean")
  	assert(lines(1) === "1,Ayeeee,False")
  	assert(lines(2) === "2 ,Bee ,True")
  	assert(lines(3) === "3,Sea,False")
  	assert(lines(4) === "4 ,Dea ,False")
  	assert(lines.size === 5)
  }
  
  @Test def exceptionIfWriteWithNonMatchingHeader {
  	val testHeader = Seq("Int", "String", "Boolean")
  	val testData1 = 
"""1,Ayeeee,False
2 ,Bee ,True
"""
  	CSVFile.write(file, testData1.lines.toIterable, header = testHeader)
  	
  	intercept[AssertionError]{
  		CSVFile.write(file, Iterable("line1", "line2"), Seq("OneBigColumn"), StandardOpenOption.APPEND)
  	}
  }
  
  @Test def overwritWithFewerLinesRemovesAllPreviousLines {
  	val header = Seq("Numbers")
    val data1 = Seq("1","2","3")
    val data2 = Seq("1","2")
      
    CSVFile.write(file, data1, header)
    CSVFile.write(file, data2, header)

    val writtenLines = Source.fromFile(file.toFile).getLines.toIndexedSeq
    
    assert(writtenLines(0) === "Numbers")
    assert(writtenLines(1).toInt === 1)
    assert(writtenLines(2).toInt === 2)
    assert(writtenLines.length === 3)
  }
}