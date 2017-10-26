package sampler.example.r

import org.rosuda.REngine.Rserve.RConnection
import play.api.libs.json.Json
import sampler.r.rserve.RServeHelper

object RServeHelperExample extends App {
  //TODO retest with jsonLite in R script

	// Prep some data to send to R
	val json = Json.obj(
		"SomeText" -> "Hello, JSON!",
		"ObsId"-> (1 to 100),
		"Value"-> (1 to 100).map(_ => math.random)
  )
	println(Json.prettyPrint(json))
	
	RServeHelper.ensureRunning()
	val connection = new RConnection
	
	//Send the data, concat strings in R, get result back
	connection.assign("jsonInR", Json.stringify(json))
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
	val jsonResult = Json.parse(res.asString)
	println(Json.prettyPrint(jsonResult))
	
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
	
	RServeHelper.shutdown()
}
