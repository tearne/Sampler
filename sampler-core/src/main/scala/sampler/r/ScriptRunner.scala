package sampler.r

import java.nio.file.Path
import java.io.FileWriter
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.io.Source

// Should QuickPlot be in companion object?

class ScriptRunner {
	def apply(script: String, scriptPath: Path){
		val writer = new FileWriter(scriptPath.toFile)
		val parentPath = scriptPath.toAbsolutePath.getParent.toFile
		
		val fullScript = new StringBuilder()
		
		fullScript.append("setwd(\""+parentPath+"\")\n")
		fullScript.append("myFunction <- function() {\n")
		fullScript.append(script + "\n")
		fullScript.append("}\ninvisible(suppressMessages(myFunction()))")
		
		writer.write(fullScript.toString)
		writer.close
		
		import scala.sys.process._
		val processOutcome = Seq("/usr/bin/Rscript", scriptPath.toString()).!
		
		if(processOutcome != 0) throw new ScriptRunnerException("An error has occured whilst running the R script")
	}
}

object ScriptRunner{
	lazy val instance = new ScriptRunner
	def apply(script: String, scriptTarget: Path) = instance(script, scriptTarget) 
}

class ScriptRunnerException(msg: String) extends RuntimeException(msg)