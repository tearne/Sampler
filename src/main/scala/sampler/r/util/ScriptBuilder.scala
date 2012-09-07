package sampler.r.util
import sampler.data.Types.Column
import sampler.data.Types.Factor

class ScriptBuilder {

	def apply(independent: IndexedSeq[Column[Factor]], dependent: Column[Double], fileName: String, outputFile: String): String = {
		
		val script = new StringBuilder
		
		script.append("library(\"rjson\")\n")
		
		script.append("data=read.csv(\"" + fileName + "\")\n")
		
		independent map {
			case a => script.append(a.name.trim + "=data$" + a.name.trim + "\n")
		}
		
		script.append(dependent.name.trim + "=data$" + dependent.name.trim + "\n")
		
		script.append("lm1=lm(")
		script.append(dependent.name.trim)
		script.append("~")
		
		independent map {
			case a if (independent.indexOf(a) < independent.length-1) => script.append(a.name.trim + "+")
			case a => script.append(a.name.trim)
		}
		
		script.append(")\n")
		
		script.append("result <- anova(lm1)\n")
		script.append("params <- row.names(result)\n")
		script.append("colNames <- names(result)\n")

		script.append("anovaJSON <- toJSON(c(format(as.data.frame(params)), format(as.data.frame(colNames)), result), method=\"C\")\n")

		script.append("fileName <- file(\"" + outputFile + "\")\n")
		script.append("writeLines(anovaJSON, fileName)\n")
		script.append("close(fileName)\n")
		
		script.toString
	}
}