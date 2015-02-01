package sampler.example.r

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import sampler.r.rserve.RServeHelper
import org.rosuda.REngine.Rserve.RConnection

object RServeHelperExample extends App {
	// Prep some data to send to R
	val json = 
		("SomeText" -> "Hello, JSON!") ~
		("ObsId"-> (1 to 100)) ~
		("Value"-> (1 to 100).map(_ => math.random))
	println(pretty(render(json)))
	
	RServeHelper.ensureRunning()
	val connection = new RConnection
	
	//Send the data, concat strings in R, get result back
	connection.assign("jsonInR", compact(render(json)))
	val script1 = 
		"""
			library(rjson)
			parsed = fromJSON(jsonInR)
			paste("R says:", parsed$SomeText)
	  """
	println(connection.parseAndEval(script1).asString())
	
	//Build dataframe in R, summarise, get results back
	val script2 = """
		  df = as.data.frame(parsed[c("ObsId","Value")])
		  toJSON(summary(df))
		"""
	val res = connection.parseAndEval(script2)
	val jsonResult = parse(res.asString)
	println(pretty(render(jsonResult)))
	
	//Always close the connection
	connection.close
	
	//Parallel calculation (new connection per thread)
	val means = (1 to 10).map{_ =>
		val c = new RConnection
		val m = c.parseAndEval("mean(runif(100, 0, 1))").asDouble()
		c.close
		m
	}
	println(s"Scala mean of R means: ${means.sum / means.size}")
}