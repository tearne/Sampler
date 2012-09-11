package sampler.r.wrap

import java.nio.file.Path
import java.nio.file.Paths
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.File
import scala.collection.mutable.HashMap
import com.typesafe.config.ConfigFactory
import sampler.data.Types._
import sampler.r.ScriptRunner
import sampler.r.util.ScriptBuilder
import sampler.io.CSVTableWriter
import sampler.r.util.JsonReader

class Anova(rExePath: Path, numLevels: Int = 4){
	
	case class Bin(lower: Double, upper: Double)
	
	def listToBinForInt(list: Seq[Int]): Bin = {
		val bin = Bin(list.min, list.max)
		bin
	}
	
	def listToBinForDouble(list: Seq[Double]): Bin = {
		val bin = Bin(list.min, list.max)
		bin
	}
	
	def int2Factor(tc: Column[Int]): Column[Factor] = {
		val numPerBin = (tc.values.size.toDouble / numLevels.toDouble).ceil.toInt
		if(numPerBin ==1)
			throw new RuntimeException("Cannot perform ANOVA when number of levels is the same as the number of data points") // R crashes
		
		val sortedGroups = tc.values.sortWith(_ < _).grouped(numPerBin).toList
		
		val binList : List[Bin] = sortedGroups.map{_ match{
			case myList => listToBinForInt(myList)
		}}
		
		var paramList: IndexedSeq[Factor] = IndexedSeq()
		
		for(value <- tc.values) {
				for(i <- 0 until binList.size) {
					val toTest = value
					val lower = binList(i).lower
					var upper = binList(i).upper+1

					try {
						upper = binList(i+1).lower
					}
					catch {
					  case ioobe: IndexOutOfBoundsException =>
					}
					
					val result = (toTest) match {
					  	case a if(toTest >= lower && toTest < upper) => paramList = paramList :+ Factor(i.toString)
						case _ => 
					}
				}
			}
		
		val returnTC = new Column(paramList, tc.name)
		
		returnTC
	}
	
	def double2Factor(tc: Column[Double]): Column[Factor] = {
		val numPerBin = (tc.values.size.toDouble / numLevels.toDouble).ceil.toInt
		if(numPerBin ==1)
			throw new RuntimeException("Cannot perform ANOVA when number of levels is the same as the number of data points") // R crashes
		
		val sortedGroups = tc.values.sortWith(_ < _).grouped(numPerBin).toList
		
		val binList : List[Bin] = sortedGroups.map{_ match{
			case myList => listToBinForDouble(myList)
		}}
		
		var paramList: IndexedSeq[Factor] = IndexedSeq()
		
		for(value <- tc.values) {
				for(i <- 0 until binList.size) {
					val toTest = value
					val lower = binList(i).lower
					var upper = binList(i).upper+1

					try {
						upper = binList(i+1).lower
					}
					catch {
					  case ioobe: IndexOutOfBoundsException =>
					}
					
					val result = (toTest) match {
					  	case a if(toTest >= lower && toTest < upper) => paramList = paramList :+ Factor(i.toString)
						case _ => 
					}
				}
			}
		
		val returnTC = new Column(paramList, tc.name)
		
		returnTC
	}
	
	def apply(independent: IndexedSeq[Column[_]], dependent: Column[Double]): AnovaResults = {
		val factorisedColumns: IndexedSeq[Column[Factor]] = independent.map{_ match{
			case IntColumn(tc) => int2Factor(tc) 
			case DoubleColumn(tc) =>  double2Factor(tc)
		}}
		
		val mainPath = Paths.get("")
		val fullPath = Paths.get(new File("").getAbsolutePath())
		
		val dataPath = mainPath.resolve("testData")
		val dataFilePath = dataPath.resolve("data.csv")
		
		// Writing data file

		val colsToWrite = factorisedColumns :+ dependent
		
		val csvTableWriter = new CSVTableWriter(dataFilePath, false)
		csvTableWriter.apply(colsToWrite:_*)
		
		// Using new ScriptRunner class
		
		val rScriptPath = dataPath.resolve("script.txt")

		val scriptBuilder = new ScriptBuilder
		
		val script = scriptBuilder.apply(factorisedColumns, dependent, dataFilePath.getFileName().toString, "anova_JSON.txt")
		
		val scriptRunner = new ScriptRunner
		
		scriptRunner.apply(script, rScriptPath)
		
		// Read in JSON output
		
		val jsonPath = dataPath.resolve("anova_JSON.txt");
		
		val jsonReader = new JsonReader
		val anovaResults = jsonReader.apply(jsonPath)

		var min = 10.0
		
		anovaResults.paramEntries.map {
			case a if(a.fValue < min) => min = a.fValue
		}
		
		anovaResults.paramEntries.map {
			case a => {
				print(a.name + ": ")
				val numStars = (a.fValue/min).asInstanceOf[Int]
				for(i <- 0 until numStars)
					print("*")
				print("\t" + a.fValue)
				print("\n")
			}
		}
		
		anovaResults
	}
}

case class AnovaResults(val paramEntries: Seq[AnovaEntry])

case class AnovaEntry(name: String, degreesFreedom: Int, sumSquares: Double, meanSquares: Double, fValue: Double, pValue: Double)
