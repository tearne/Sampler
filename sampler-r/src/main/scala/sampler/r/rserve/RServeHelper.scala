package sampler.r.rserve

import scala.annotation.tailrec
import scala.util.Try
import org.rosuda.REngine.Rserve.RConnection
import scala.util.Success
import scala.util.Failure
import java.net.ConnectException
import scala.sys.process.Process
import java.nio.file.Path
import java.nio.file.Files
import org.rosuda.REngine.REXP

object RServeHelper {
	@tailrec
	def getConnection(countdown: Int): Try[RConnection] = {
		if(countdown == 0) Failure(new ConnectException("Could not connect to Rserve"))
		else try{
			val c = new RConnection()
			println("Rserve connection confirmed")
			Success(c)
		} catch {
			case _: Exception => 
				val newCountdown = countdown - 1
				println(s"Searching for Rserve ($newCountdown)")
				Thread.sleep(100)
				getConnection(newCountdown)
		}
	}
	
	def ensureRunning(){
		def isRunning() = {
			val attempts = 5
			
			getConnection(attempts).map{c => 
				c.close(); 
				true
			}
			.getOrElse(false)
		}
		
		if(!isRunning){
			println("Starting new Rserve process")
			Process("R CMD Rserve --no-save --slave").run
			assert(isRunning)
		}
	}
	
	def runScript(script: String, saveToFile: Option[Path] = None): Try[REXP] = {
		saveToFile.map{path => 
			val writer = Files.newBufferedWriter(path)
			writer.write(script)
			writer.newLine
			writer.close
		}
		
		getConnection(1).map{connection =>
			try{
				connection.parseAndEval(script)
			}finally{
				connection.close
			}
		}
	}
	
	def shutdown() = new RConnection().shutdown()
}