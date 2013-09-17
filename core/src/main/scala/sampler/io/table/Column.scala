/*
 * Copyright (c) 2012-13 Crown Copyright 
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

case class Column[T](val values: Seq[T], val name: String)(implicit val cType: ColumnType[T]){
	def toStringColumn(): Column[String] = Column(values.map(v => cType.toString(v)), name)
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