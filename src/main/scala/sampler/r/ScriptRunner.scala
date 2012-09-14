package sampler.r

import java.nio.file.Path
import java.io.FileWriter
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.io.Source

class ScriptRunner {
	def apply(script: String, scriptTarget: Path){
		val writer = new FileWriter(scriptTarget.toFile)
		val parentPath = scriptTarget.toAbsolutePath.getParent.toFile
		
		writer.write(
			"setwd(\""+parentPath+"\")\n" + script
		)
		writer.close
		
		val builder = new StringBuilder()
		builder.append("R CMD BATCH --slave ")
		builder.append(scriptTarget.getFileName().toFile)
		
		val proc = Runtime.getRuntime().exec(builder.toString, null, parentPath)
		proc.waitFor()
		
		val rOutFile = scriptTarget.getFileName().toString() + ".Rout"
		
		var outPath = scriptTarget.getParent().resolve(rOutFile)

		val source = Source.fromFile(outPath.toString).mkString
		
		if(source.startsWith("Error:"))
			throw new ScriptRunnerException("An error has occured whilst running the R script")
		
		val stdInput = new BufferedReader(new 
				InputStreamReader(proc.getInputStream())
		)

        val stdError = new BufferedReader(new 
        		InputStreamReader(proc.getErrorStream())
        )

		//TODO Logging rather than println-ing
        System.out.println("std out:");
        Source.fromInputStream(proc.getInputStream()).getLines.foreach(println)

        System.out.println("std err:");
        Source.fromInputStream(proc.getErrorStream()).getLines.foreach(println)
	}
}

object ScriptRunner{
	lazy val instance = new ScriptRunner
	def apply(script: String, scriptTarget: Path) = instance(script, scriptTarget) 
}

class ScriptRunnerException(msg: String) extends RuntimeException(msg)