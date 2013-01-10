package eu9302.winbugs

import scala.io.Source
import java.io.File

object ChainReader{
  def apply(fileStem: String) = {
	val indexLines = Source.fromFile(new File(fileStem+"Index.txt")).getLines.toIterator
	val dataList = Source.fromFile(new File(fileStem+"1.txt")).getLines.toIterator.map{line =>
		line.split("\\t")(1)
	}.toList
	
	val index = collection.mutable.Map[String, (Int, Int)]()
	
	indexLines.foreach{line =>
		val tokens = line.split("\\t")
		index.put(tokens(0), (Integer.parseInt(tokens(1)),Integer.parseInt(tokens(2))))
	}
	
	val chains = collection.mutable.Map[String, List[Double]]()
	index.foreach{ case (key,(start, stop)) =>
		chains.put(key, dataList.drop(start-1).take(stop-start+1).map(java.lang.Double.parseDouble _))
	}
	
	chains
  }
  
  def main(args: Array[String])={
    //apply("WinBUGS/model/output").foreach(println _)
    
//    val data = apply("WinBUGS/model/output")
//    
//  //Read the chain data and spew out the Se/Sp info in column format
//    val writer = new Writer(new File("data/Scala_out/SeSpChain.csv"))
//    writer.addColumn(data("SeH"), "SeH")
//    writer.addColumn(data("SpH"), "SpH")
//    writer.addColumn(data("SeE"), "SeE")
//    writer.addColumn(data("SpE"), "SpE")
//    writer.write()
  }
}