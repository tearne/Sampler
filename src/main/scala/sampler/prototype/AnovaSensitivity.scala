/*
 * Copyright (c) 2012 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.prototype

import java.nio.file.Path
import java.nio.file.Paths
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.File
import scala.collection.mutable.HashMap
import com.typesafe.config.ConfigFactory
import sampler.data.Types._

class AnovaRunner(rExePath: Path, numLevels: Int = 4){
	
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
		
		val dataPath = mainPath.resolve("data.csv")
		
		// Writing data file

		val dataFile = new FileOutputStream(dataPath.toString)
		val dataStream = new PrintStream(dataFile)

		val nIterations = dependent.values.length
		val nParameters = independent.length

		for(i <- 0 until factorisedColumns.length){
			dataStream.print(factorisedColumns(i).name.getOrElse("iv"+1))
			dataStream.print(",")
		}
		
		dataStream.print(dependent.name.getOrElse("depVar"))
		dataStream.println
		
		for(i <- 0 until nIterations) {
			for(j <- 0 until nParameters) {
				dataStream.print(factorisedColumns(j).values(i).toString().trim())
				dataStream.print(",")
			}
			dataStream.print(dependent.values(i))
			dataStream.println
		}

		dataStream.close
		
		// ADD writer for R script
		
		val rScriptPath = mainPath.resolve("script.txt")

		val scriptFile = new FileOutputStream(rScriptPath.toString)
		val scriptStream = new PrintStream(scriptFile)

		scriptStream.println("library(\"rjson\")")

		scriptStream.print("setwd(\"")

		//	NOTE: R requires / not \ as in path hence the need to iterate through path for stream writing

		val root = fullPath.getRoot.toString
		val trimmedRoot = root.replace("\\", "")

		scriptStream.print(trimmedRoot + "/")

		for(i <- 0 until fullPath.getNameCount()) {
			scriptStream.print(fullPath.getName(i) + "/")
		}

		scriptStream.print("\")\n")
		scriptStream.println("data=read.csv(\"" + "data.csv" + "\")")

		for(i <- 0 until nParameters){
			val paramName = factorisedColumns(i).name.getOrElse("iv"+i)
			
			scriptStream.println(paramName.trim() + "=data$" + paramName.trim())
		}

		val depVarName = dependent.name.getOrElse("depVar")
		
		scriptStream.println(depVarName.trim() + "=data$" + depVarName.trim())

		scriptStream.print("lm1=lm(")
		scriptStream.print(depVarName.trim())
		scriptStream.print("~")
		for(i <- 0 until nParameters){
			val paramName = factorisedColumns(i).name.getOrElse("iv"+i)
			scriptStream.print(paramName.trim())
			if(i<nParameters-1)
				scriptStream.print("+")
		}
		scriptStream.print(")\n")

		scriptStream.println("result <- anova(lm1)")
		scriptStream.println("params <- row.names(result)")
		scriptStream.println("colNames <- names(result)")

		scriptStream.println("anovaJSON <- toJSON(c(format(as.data.frame(params)), format(as.data.frame(colNames)), result), method=\"C\")")

		scriptStream.println("fileName <- file(\"anova_JSON.txt\")")
		scriptStream.println("writeLines(anovaJSON, fileName)")
		scriptStream.println("close(fileName)")

		scriptStream.close
		
		// Run script in R
		
		// Mac OS X
		val process = Runtime.getRuntime().exec("Rscript script.txt")
		
		// Windows
		// val process = Runtime.getRuntime().exec(rExePath.toString + " CMD BATCH --slave script.txt")
		
		process.waitFor
		
		// Read in JSON output
		
		val jsonPath = mainPath.resolve("anova_JSON.txt");

		val config = ConfigFactory.parseFile(jsonPath.toFile())

		val params = config.getStringList("params")
		val colNames = config.getStringList("colNames")
		val degrees = config.getIntList("Df")
		val sumSqs = config.getDoubleList("Sum Sq")
		val meanSqs = config.getDoubleList("Mean Sq")
		val fValues = config.getAnyRefList("F value")
		val pValues = config.getAnyRefList("Pr(>F)")

		var resultsMap: IndexedSeq[AnovaEntry] = IndexedSeq()

		var min = fValues.get(0).asInstanceOf[Double]

		for(i <- 0 until params.size()) {
			if(fValues.get(i) == "NA") {
				//		  resultsMap += params.get(i) -> new ANOVA(degrees.get(i), sumSqs.get(i), meanSqs.get(i), 0, 0)
			} else {
				val fValue : Double = fValues.get(i).asInstanceOf[Double]
				val pValue : Double = pValues.get(i).asInstanceOf[Double]

				if(fValue < min)
					min = fValue

				val entry = new AnovaEntry(params.get(i), degrees.get(i), sumSqs.get(i), meanSqs.get(i), fValue, pValue)
				resultsMap = resultsMap :+ entry
			}
		}

		resultsMap.foreach{case (entry) => {
			print(entry.name + ": ")
			val numStars = (entry.fValue/min).asInstanceOf[Int]
			for(i <- 0 until numStars)
				print("*")
			print("\t" + entry.fValue)
			print("\n")
			}
		}
		
		val anovaResults = new AnovaResults(resultsMap)
		
		anovaResults
	}
}

class AnovaResults(val paramEntries: IndexedSeq[AnovaEntry]){
}

case class AnovaEntry(name: String, degreesFreedom: Int, sumSquares: Double, meanSquares: Double, fValue: Double, pValue: Double)
