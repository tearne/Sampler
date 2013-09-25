package sampler.r.anova

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.io.table._

//TODO If specific to Anova then put alongside Anova tests in dedicated package

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
			
			val expectedScript =
"""library("rjson")
data=read.csv("data.csv")
C1=data$C1
C2=data$C2
result=data$result
lm1=lm(result~C1+C2)
result <- anova(lm1)
params <- row.names(result)
colNames <- names(result)
anovaJSON <- toJSON(c(format(as.data.frame(params)), format(as.data.frame(colNames)), result), method="C")
fileName <- file("jsonFile.txt")
writeLines(anovaJSON, fileName)
close(fileName)
"""

			rScript mustEqual expectedScript	
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

			val rScript = scriptBuilder.apply(IndexedSeq(c1,c2, c3, c4), d1, dataFile, jsonOutputFile)

			val expectedScript =
"""library("rjson")
data=read.csv("differentData.csv")
P1=data$P1
P2=data$P2
P3=data$P3
P4=data$P4
dependent_values=data$dependent_values
lm1=lm(dependent_values~P1+P2+P3+P4)
result <- anova(lm1)
params <- row.names(result)
colNames <- names(result)
anovaJSON <- toJSON(c(format(as.data.frame(params)), format(as.data.frame(colNames)), result), method="C")
fileName <- file("differentJSON.txt")
writeLines(anovaJSON, fileName)
close(fileName)
"""
			
			rScript mustEqual expectedScript
		}
	}
}