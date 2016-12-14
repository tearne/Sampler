/*
 * Copyright (c) 2012-15 Crown Copyright
 *                       Animal and Plant Health Agency
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

import java.io.FileWriter
import java.nio.file.{Files, Path}

trait RScript {
	def apply(script: String, workingDirectory: Path)
}

/** Allows saving and running of an R script, eliminating concerns on working directory
 *  and allowing the saved scripts to be easily debugged on the command line.
 *  */
object RScript extends RScript{
	/** Writes and R script to the location defined by path, adding a line to ensure
	 *  the script will be run from the correct working directory. Executes the script using 
	 *  the Rscript shell script
	 * 	
	 *  @param script The R script to be executed
	 *  @param Path to save the script to and where it will be run from
	 */
	def apply(script: String, scriptPath: Path){
		val wd = scriptPath.getParent
		assert(Files.exists(wd), s"Working dir ${wd.toAbsolutePath()} doesn't exist")
		
		val writer = new FileWriter(scriptPath.toFile)
		
		val fullScript = new StringBuilder()
		
		fullScript.append("setwd(\""+wd.toAbsolutePath()+"\")\n")
		fullScript.append(script + "\n")
		
		writer.write(fullScript.toString)
		writer.close
		
		import scala.sys.process._
		val processOutcome = Seq("/usr/bin/Rscript", scriptPath.toString()).!
		
		if(processOutcome != 0) throw new RScriptException("An error has occured whilst running the R script")
	}
}

class RScriptException(msg: String) extends RuntimeException(msg)