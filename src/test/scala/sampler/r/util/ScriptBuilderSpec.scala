package sampler.r.util

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.data.Types.Column
import sampler.data.Types.Factor

@RunWith(classOf[JUnitRunner])
class ScriptBuilderSpec extends Specification{

	"ScriptBuilder" should {
		"write an R script to perform an ANOVA in R" in {
			val c1 = Column(Seq(Factor("one"), Factor("two"), Factor("three")), "C1")
			val c2 = Column(Seq(Factor("aye"), Factor("bee"), Factor("sea")), "C2")

			val d1 = Column(Seq(0.2,0.3,0.4), "result")
			
			val dataFile = "data.csv"
			val jsonOutputFile = "jsonFile.txt"
				
			val scriptBuilder = new ScriptBuilder
			
			val rScript = scriptBuilder.apply(IndexedSeq(c1,c2), d1, dataFile, jsonOutputFile)
			
			val rLines = rScript split "\n"
			
			(rLines(0) mustEqual "library(\"rjson\")") and
			(rLines(1) mustEqual "data=read.csv(\"data.csv\")") and
			(rLines(2) mustEqual "C1=data$C1") and
			(rLines(3) mustEqual "C2=data$C2") and
			(rLines(4) mustEqual "result=data$result") and
			(rLines(5) mustEqual "lm1=lm(result~C1+C2)") and
			(rLines(6) mustEqual "result <- anova(lm1)") and
			(rLines(7) mustEqual "params <- row.names(result)") and
			(rLines(8) mustEqual "colNames <- names(result)") and
			(rLines(9) mustEqual "anovaJSON <- toJSON(c(format(as.data.frame(params)), format(as.data.frame(colNames)), result), method=\"C\")") and
			(rLines(10) mustEqual "fileName <- file(\"anova_JSON.txt\")") and
			(rLines(11) mustEqual "writeLines(anovaJSON, fileName)") and
			(rLines(12) mustEqual "close(fileName)")
		}
		
		"be able to write a script with varying file names and numbers of parameters" in {
			val c1 = Column(Seq(Factor("one"), Factor("two"), Factor("three")), "P1")
			val c2 = Column(Seq(Factor("aye"), Factor("bee"), Factor("sea")), "P2")
			val c3 = Column(Seq(Factor("another"), Factor("table"), Factor("column")), "P3")
			val c4 = Column(Seq(Factor("fourth"), Factor("parameter"), Factor("set")), "P4")

			val d1 = Column(Seq(0.2,0.3,0.4), "dependent_values")

			val dataFile = "differentData.csv"
			val jsonOutputFile = "differentJSON.txt"

			val scriptBuilder = new ScriptBuilder

			val rScript = scriptBuilder.apply(IndexedSeq(c1,c2), d1, dataFile, jsonOutputFile)

			val rLines = rScript split "\n"

			(rLines(1) mustEqual "data=read.csv(\"differentData.csv\")") and
			(rLines(2) mustEqual "P1=data$P1") and
			(rLines(3) mustEqual "P2=data$P2") and
			(rLines(4) mustEqual "P3=data$P3") and
			(rLines(5) mustEqual "P4=data$P4") and
			(rLines(7) mustEqual "lm1=lm(result~P1+P2+P3+P4)") and
			(rLines(10) mustEqual "fileName <- file(\"differentJSON.txt\")")
		}
	}

	/*	FULL R SCRIPT FOR FIRST TEST
		library("rjson")
		data=read.csv("data.csv")
		C1=data$C1
		C2=data$C2
		result=data$result
		lm1=lm(result~C1+C2)
		result <- anova(lm1)
		params <- row.names(result)
		colNames <- names(result)
		anovaJSON <- toJSON(c(format(as.data.frame(params)), format(as.data.frame(colNames)), result), method="C")
		fileName <- file("anova_JSON.txt")
		writeLines(anovaJSON, fileName)
		close(fileName)
	*/
}