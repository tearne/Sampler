package sampler.r.anova

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mock.Mockito
import sampler.r.ScriptRunner
import java.nio.file.Paths
import sampler.columnbasedtable.CSVTableWriter
import sampler.columnbasedtable.Column

@RunWith(classOf[JUnitRunner])
class AnovaSpec extends Specification with Mockito {
	
	val csvTableWriter = mock[CSVTableWriter]
	val scriptBuilder = mock[ScriptBuilder]
	val scriptRunner = mock[ScriptRunner]
	val jsonReader = mock[JsonReader]
	val fakeRpath = Paths.get("")
	val numLevels = 4
	
	val anova = new Anova(csvTableWriter, scriptBuilder, scriptRunner, jsonReader, fakeRpath, numLevels)
	
	"Anova" should {
		"Execute the entire process of running an ANOVA by calling various scripts and classes" in {
			val c1 = Column(Seq(1,2,3,4,5,6), "C1")
			val c2 = Column(Seq(1,2,1,3,2,1), "C2")
			val d1 = Column(Seq(2.0,4,6,7,8,9), "result")
			
			val c1factor = anova.int2Factor(c1)
			val c2factor = anova.int2Factor(c2)
			
			val colsToWrite = IndexedSeq(c1factor, c2factor, d1)
			
			val homePath = Paths.get("")
			val targetPath = homePath.resolve("testData")
			val rScriptPath = targetPath.resolve("script.txt")
			val jsonPath = targetPath.resolve("anova_JSON.txt");
			
			val dummyScript = "Dummy Script"
			
			scriptBuilder.apply(IndexedSeq(c1factor, c2factor), d1, "data.csv", "anova_JSON.txt") returns dummyScript
			
			anova.apply(IndexedSeq(c1,c2), d1, targetPath)
			
//			Fix arguments and verifys
			
			(there was one(csvTableWriter).apply(colsToWrite:_*)) and
			(there was one(scriptBuilder).apply(IndexedSeq(c1factor, c2factor), d1, "data.csv", "anova_JSON.txt")) and
			(there was one(scriptRunner).apply(dummyScript, rScriptPath)) and
			(there was one(jsonReader).apply(jsonPath))
		}
	}

}