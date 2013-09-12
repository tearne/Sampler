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

package sampler.io.table

import sampler.math.Probability

// TODO: Refactor into column class? Don't like the fact that it's hidden in Types.scala

object Types {
	case class Header[T](val name: String)(implicit val cType: ColumnType[T])  
	case class Column[T](val values: Seq[T], val name: String)(implicit val cType: ColumnType[T]){
		def toStringColumn(): Column[String] = Column(values.map(v => cType.toString(v)), name)
	}
	
	//TODO need to have a think about what should constitute a factor.
	//     This impl seems a bit bare, but has been fine so far.  Perhaps 
	//	   classes of factors should be aware of the legal set if values?
	case class Factor(name: String){
		override def toString() = name
	}
	
	abstract class ColumnType[T: Manifest]{ 
		val m = manifest[T]
		def apply(s: String):T
		def toString(value: T): String = value.toString
		def unapply(col: Column[_]): Option[Column[T]] = {
			if(col.cType.m == m) Some(col.asInstanceOf[Column[T]])
			else None
		}
	}
	implicit val IntColumn: ColumnType[Int] = new ColumnType[Int] { def apply(s: String) = s.toInt }
	implicit val DoubleColumn: ColumnType[Double] = new ColumnType[Double] { def apply(s: String) = s.toDouble }
	implicit val BooleanColumn: ColumnType[Boolean] = new ColumnType[Boolean] { def apply(s: String) = s.toBoolean }
	implicit val StringColumn: ColumnType[String] = new ColumnType[String] { def apply(s: String) = s }
	implicit val FactorColumn: ColumnType[Factor] = new ColumnType[Factor] { def apply(s: String) = Factor(s) }
	implicit val ProbabilityColumn: ColumnType[Probability] = new ColumnType[Probability] { 
		def apply(s: String) = Probability(s.toDouble)
				override def toString(p: Probability) = p.value.toString
	}
}