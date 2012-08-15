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

class AnovaRunner(rExePath: Path, numLevels: Int = 4){
	
	trait Factor{
		val name: String
	}
	
	def int2Factor(tc: TableColumn[Int]): TableColumn[Factor] = {println("1"); null}
	def double2Factor(tc: TableColumn[Double]): TableColumn[Factor] = {println("2"); null}
	
	def apply(independent: IndexedSeq[TableColumn[_]], dependent: IndexedSeq[Double]): AnovaResults = {
		import TableColumnMatcher._
		val factorisedColumns: IndexedSeq[TableColumn[Factor]] = independent.map{_ match{
			case IntTC(tc) => int2Factor(tc) 
			case DoubleTC(tc) =>  double2Factor(tc)
		}}
		null
	}
}

class AnovaResults{
	//def factors: Table = null  
}

