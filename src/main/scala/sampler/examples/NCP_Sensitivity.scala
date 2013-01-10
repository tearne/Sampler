package sampler.examples

import java.nio.file.Paths
import eu9302.winbugs.ChainReader
import sampler.data.Empirical._

object NCP_Sensitivity extends App{
  
  val home = Paths.get("", "examples", "ncpSampleSize", "data", "coda")
  
//  println(home.toAbsolutePath())
  
  val chains = ChainReader(home.toString())
  
  val requiredParameters = List(
	"PPosEUFaecesCage[1]",
	"PPosEUFaecesCage[2]",
	"PPosEUFaecesCage[3]",
	"PPosEUFaecesCage[4]",
	"PPosEUFaecesCage[5]",
	"PPosEUFaecesCage[6]"
  )
  
  val distMap = requiredParameters map (name => name -> (chains.get(name).get).toIndexedSeq.toEmpiricalTable) toMap

  
}