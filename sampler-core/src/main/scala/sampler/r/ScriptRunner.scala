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

package sampler.r

import java.io.FileWriter
import java.nio.file.Path

import scala.sys.process.stringSeqToProcess

/** A class to allow the running of an R script from within the Scala environment*/
class ScriptRunner {
	/** Writes and R script to the location defined by path. Executes the script using 
	 *  the Rscript shell script
	 * 	
	 *  @param script The R script to be executed
	 *  @param scriptPath The path to where the script should be written, including filename
	 */
	def apply(script: String, scriptPath: Path){
		val writer = new FileWriter(scriptPath.toFile)
		val parentPath = scriptPath.toAbsolutePath.getParent.toFile
		
		val fullScript = new StringBuilder()
		
		fullScript.append("setwd(\""+parentPath+"\")\n")
		fullScript.append(script + "\n")
		
		writer.write(fullScript.toString)
		writer.close
		
		import scala.sys.process._
		val processOutcome = Seq("/usr/bin/Rscript", scriptPath.toString()).!
		
		if(processOutcome != 0) throw new ScriptRunnerException("An error has occured whilst running the R script")
	}
}

/** Companion object to allow running of an R script*/
object ScriptRunner{
	lazy val instance = new ScriptRunner
	def apply(script: String, scriptTarget: Path) = instance(script, scriptTarget) 
}

class ScriptRunnerException(msg: String) extends RuntimeException(msg)