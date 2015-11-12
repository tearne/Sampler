/*
 * Copyright (c) 2012-15 Crown Copyright 
 * Animal & Plant Health Agency
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

package sampler.r.script

import java.nio.file.{Paths, Files}
import collection.JavaConversions.asScalaBuffer
import java.nio.file.Path
import org.scalatest.BeforeAndAfter
import org.scalatest.FreeSpec
import scala.io.Source
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterAll

@RunWith(classOf[JUnitRunner])
class RScriptTest extends FreeSpec with BeforeAndAfter with BeforeAndAfterAll {

  val baseDir = Paths.get(getClass.getResource("").toURI()).resolve("shouldBeDeletedByTest")
  
	val scriptPath = baseDir.resolve("script.r")
	val rOutPath = baseDir.resolve("script.r.Rout")
	val jsonPath = baseDir.resolve("script.json")
	val doesntExist = baseDir.resolve("doesntExist")
	
	"Prepend script with working dir" in {
  	RScript("two = 1 + 1", baseDir)
  	val expectedLines = Array(
  			s"""setwd("${baseDir.toAbsolutePath()}")""",
				"two = 1 + 1"
  	)
  	val writtenLines = Source.fromFile(scriptPath.toFile()).mkString.split("\n")
  	
  	expectedLines.zip(writtenLines).foreach{case (e,a) => assert(e === a)}
  }
	
	"Running sleep command" in {
	  val startTime = System.nanoTime
	  RScript("Sys.sleep(1)", baseDir)
    val runTime = (System.nanoTime() - startTime) / 1e9
			
	  assert(runTime > 1.0)
	}
	
	"ScriptRunnerException when code fails" in {
	  intercept[RScriptException] {
	    RScript("results <- toJSON(object)", baseDir)
	  }

	  intercept[RScriptException] {
			RScript("library(\"rjson\")\nresults <- toJSON(object)", baseDir)
	  }
	}
	
	"Error if directory doesn't exist" in {
	  intercept[AssertionError] {
	    RScript("a <- 1", doesntExist)
	  }
	}
	
	after {
	  List(scriptPath, rOutPath, jsonPath).foreach(Files.deleteIfExists)
	}
	
	override def beforeAll {
		Files.createDirectories(baseDir)
	}
	
	override def afterAll {
		Files.deleteIfExists(baseDir)
	}
}