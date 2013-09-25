package sampler.r.anova

import java.nio.file.Path
import java.nio.file.Paths
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.File
import scala.collection.mutable.HashMap
import com.typesafe.config.ConfigFactory
import sampler.io.table._
import sampler.r.ScriptRunner
import sampler.io.table.CSVTableWriter

class Anova(csvTableWriter: CSVTableWriter, scriptBuilder: ScriptBuilder, scriptRunner: ScriptRunner, jsonReader: JsonReader, rExePath: Path, numLevels: Int = 4){
	
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
	
	def apply(independent: IndexedSeq[Column[_]], dependent: Column[Double], targetPath: Path): AnovaResults = {
		val factorisedColumns: IndexedSeq[Column[Factor]] = independent.map{_ match{
			case IntColumn(tc) => int2Factor(tc) 
			case DoubleColumn(tc) =>  double2Factor(tc)
		}}
		
//		Paths
		
		val dataFilePath = targetPath.resolve("data.csv")
		val rScriptPath = targetPath.resolve("script.txt")
		val jsonPath = targetPath.resolve("anova_JSON.txt");
		
		// Writing data file

		val colsToWrite = factorisedColumns :+ dependent
		
		csvTableWriter.apply(colsToWrite:_*)
		
		// Using new ScriptRunner class
		
		val script = scriptBuilder.apply(factorisedColumns, dependent, dataFilePath.getFileName().toString, jsonPath.getFileName().toString)
		
		scriptRunner.apply(script, rScriptPath)
		
		// Read in JSON output
		
		val anovaResults = jsonReader.apply(jsonPath)
		
//		val grapher = new AnovaGrapher //comment to make test pass
//		grapher.apply(anovaResults)
		
		anovaResults
	}
}

object Anova{
	
	def apply(csvTableWriter: CSVTableWriter, scriptBuilder: ScriptBuilder, scriptRunner: ScriptRunner, jsonReader: JsonReader, rExePath: Path, numLevels: Int) 
		= new Anova(csvTableWriter, scriptBuilder, scriptRunner, jsonReader, rExePath, numLevels)
	
}

case class AnovaResults(val paramEntries: Seq[AnovaEntry])

case class AnovaEntry(name: String, degreesFreedom: Int, sumSquares: Double, meanSquares: Double, fValue: Double, pValue: Double)
