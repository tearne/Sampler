package sampler.r.rserve

import java.io.InputStream
import java.net.ConnectException
import java.nio.file.Files
import java.nio.file.Path

import scala.annotation.tailrec
import scala.io.Source
import scala.sys.process.Process
import scala.sys.process.ProcessIO
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.language.implicitConversions

import org.rosuda.REngine.REXP
import org.rosuda.REngine.Rserve.RConnection

import sampler.io.Logging

object RServeHelper extends Logging{
	@tailrec
	def getConnection(countdown: Int): Try[RConnection] = {
		if(countdown == 0) Failure(new ConnectException("Could not connect to Rserve"))
		else try{
			val c = new RConnection()
			log.debug("Rserve connection confirmed")
			Success(c)
		} catch {
			case _: Exception => 
				val newCountdown = countdown - 1
				log.debug("Searching for Rserve {} tries left",newCountdown)
				Thread.sleep(100)
				getConnection(newCountdown)
		}
	}
	
	def ensureRunning(attempts: Int = 5, daemonizeThreads: Boolean = true){
		getConnection(attempts)
			.recoverWith{case _ => 
				startRserve(daemonizeThreads)
				getConnection(attempts)
			}
			.map(_.close())
			.recover{case _ => throw new Exception("Failed to find or start Rserve")}
			.get
	}
	
	private def startRserve(daemonizeThreads: Boolean){
		implicit def toLines(in: InputStream) = Source.fromInputStream(in).getLines
		
		log.info("Start Rserve")
		val io = new ProcessIO(
					in => in.close,
					out => {
						out.foreach(log.info)
						out.close	
					},
      		err => {
      			err.foreach(log.error)
      			err.close	
      		},
      		daemonizeThreads
				)
		Process("R CMD Rserve --no-save --slave").run(io)
	}
	
	def runScript(script: String, saveToFile: Option[Path] = None): Try[REXP] = {
		saveToFile.map{filePath => 
			val writer = Files.newBufferedWriter(filePath)
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