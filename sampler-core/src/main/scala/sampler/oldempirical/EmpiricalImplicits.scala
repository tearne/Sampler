package sampler.oldempirical;
//package sampler.empirical
//
//import scala.collection.GenSeq
//import scala.collection.GenMap
//
///** Mix-in for creating Empirical instances from standard collections
// *  
// *  An EmpiricalSeq can be created from an IndexedSeq of elements
// *  {{{
// *  > import sampler.Implicits._
// *  > val empiricalSeq = Seq(1,2,2,3,3,3).toEmpiricalSeq
// *      empiricalSeq: sampler.data.EmpiricalSeq[Int] = sampler.data.EmpiricalSeq@89c196b4
// *  }}}
// *  
// *  An EmpiricalTable could be created from a Map of element to count or an IndexedSeq of elements
// *  {{{
// *  > import sampler.Implicits._
// *  > val empiricalTable1 = Seq(1,2,2,3,3,3).toEmpiricalTable
// *      empiricalTable1: sampler.data.EmpiricalTable[Int] = sampler.data.EmpiricalTable@e132e25a
// *  
// *  > val empiricalTable2 = Map(1 -> 1, 2 -> 2, 3 -> 3).toEmpiricalTable
// *      empiricalTable2: sampler.data.EmpiricalTable[Int] = sampler.data.EmpiricalTable@e132e25a
// *  }}}
// */
//trait EmpiricalImplicits {
//  implicit class RichIndexedSeq[A](genSeq: GenSeq[A]) {
//		val indSeq = genSeq.toIndexedSeq
//		def toEmpiricalSeq = new EmpiricalSeq[A](indSeq)
//		def toEmpiricalTable = new EmpiricalTable[A](
//			indSeq.groupBy(identity).map{case (k,v) => k -> v.size}
//		)
//	}
//	
//	implicit class RichMapInt[A](table: GenMap[A,Int]) {
//		def toEmpiricalTable = {
//			if(table.values.find(_ <= 0).isDefined) throw new UnsupportedOperationException("Cannot convert to EmpiricalTable, non-positive counts found")
//			else new EmpiricalTable[A](table.seq.toMap)
//		}
//	}
//}