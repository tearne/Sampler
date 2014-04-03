package sampler.r.anova

import java.nio.file.Path
import com.typesafe.config.ConfigFactory
import java.nio.file.Files

class JsonReader {

	def apply(jsonPath: Path): AnovaResults = {
		
		if(Files.exists(jsonPath) == false) {
			throw new AnovaJsonReaderException("The JSON file containing the ANOVA results is not present")
		}
		
		val config = ConfigFactory.parseFile(jsonPath.toFile())

		val params = config.getStringList("params")
		val colNames = config.getStringList("colNames")
		val degrees = config.getIntList("Df")
		val sumSqs = config.getDoubleList("Sum Sq")
		val meanSqs = config.getDoubleList("Mean Sq")
		val fValues = config.getAnyRefList("F value")
		val pValues = config.getAnyRefList("Pr(>F)")

		import collection.JavaConversions._
		val withoutResiduals = params.filter(
			_ != "Residuals"
		)
		
		val results = withoutResiduals.map {a =>
				val i = params.indexOf(a)
				
				val fValue : Double = fValues.get(i).asInstanceOf[Double]
				val pValue : Double = pValues.get(i).asInstanceOf[Double]
				
				new AnovaEntry(params.get(i), degrees.get(i), sumSqs.get(i), meanSqs.get(i), fValue, pValue)
		}
		
		new AnovaResults(results)
	}
}

class AnovaJsonReaderException(msg: String) extends RuntimeException(msg)