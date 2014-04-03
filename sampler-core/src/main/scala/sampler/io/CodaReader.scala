/*
 * Copyright (c) 2013 Crown Copyright 
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

package sampler.io

import scala.io.Source
import java.io.File

/** Object to read in coda files of parameter estimates as produced by WinBUGS */

object CodaReader{
  /** Reads in the coda1 and codaIndex files from WinBUGS and produces a map from a String of the parameter name to
   *  the distribution of parameter estimates as a List of Doubles
   *  
   *  @param fileStem Absolute path to the data files including the common file name between the 1 and Index files; i.e. path/coda would point to coda1.txt and codaIndex.txt
   *  @return Map from parameter name to list of parameter estimates (doubles)
   */
  def apply(fileStem: String = "coda") = {
	val indexLines = Source.fromFile(new File(fileStem+"Index.txt")).getLines.toIterator
	val dataList = Source.fromFile(new File(fileStem+"1.txt")).getLines.toIterator.map{line =>
		line.split("\\t")(1)
	}.toList
	
	val index = collection.mutable.Map[String, (Int, Int)]()
	
	indexLines.foreach{line =>
		val tokens = line.split("\\t")
		index.put(tokens(0), (Integer.parseInt(tokens(1)),Integer.parseInt(tokens(2))))
	}
	
	assert(dataList.length == index.values.toSeq.map{case (min, max) => max}.max)
	
	val chains = collection.mutable.Map[String, List[Double]]()
	index.foreach{ case (key,(start, stop)) =>
		chains.put(key, dataList.drop(start-1).take(stop-start+1).map(java.lang.Double.parseDouble _))
	}
	
	chains
  }
}