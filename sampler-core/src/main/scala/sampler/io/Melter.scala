package sampler.io

import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.FileSystems
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import scala.io.Source
import java.nio.file.FileVisitResult
import java.nio.file.Paths

object Melter extends App {
	//TODO this is experimental at present
	
	
	type Table = Seq[Array[String]]
	
	val dir = Paths.get(getClass.getClassLoader.getResource("data").toURI()).resolve("melter")
	println(dir)
	Melter.dir(dir, "glob:**/*.csv", "Generation").foreach(array => println(array.toSeq))
	
	def csvToLines(path: Path): Table = Source
		.fromFile(path.toFile)
		.getLines
		.map(_.split(",")
		.map(_.trim))
		.toSeq
	
	def cBind(left: Table, right: Table): Table = {
		assume(left.size == right.size)
	
		left.zip(right)
			.map{case (l,r) => l ++: r}
			.toSeq
	}
	
	def rBind(top: Table, bottom: Table): Table = {
  	assume(top.head.toSeq == bottom.head.toSeq, s"Headers don't match: (${top.head.toSeq}) vs (${bottom.head.toSeq})")
  	
  	top ++: bottom.tail 
	}
	
  def dir(dir: Path, glob: String, fileNameHeader: String) = {
  	assume(Files.isDirectory(dir))
  	
  	val files = {
  		val matcher = FileSystems.getDefault.getPathMatcher(glob)
	  	val selected = collection.mutable.ListBuffer[Path]()
	  	Files.walkFileTree(dir, new SimpleFileVisitor[Path]{
	  		override def visitFile(path: Path, attr: BasicFileAttributes) = { 
	  			println(path)
	  			if(matcher.matches(path)) {
	  				println("matsh")
	  				selected.+=(path)
	  			}
	  			FileVisitResult.CONTINUE
	  		}
	  	})
	  	selected.toSet
  	}
  	
  	println(files)
  	
  	val dataSets: Set[Table] = files.map{path => 
  		val data = csvToLines(path)
  		val fileName = path.getFileName.toString.takeWhile(_ != '.')
  		val nameColumn = Array(fileNameHeader) +: Seq.fill[Array[String]](data.size - 1)(Array(fileName))
  		cBind(data, nameColumn)
  	} 
  	val allData = dataSets.reduceLeft((t1, t2) => rBind(t1, t2))
  	
  	val variablesIndex = allData.head.dropRight(1).zipWithIndex.toMap // rop the fileNameHeader as it will be Id variable
  	val idVars = allData.head.last
  	
  	val longFormat = Array("variable", "value", fileNameHeader) +:
	  	allData.tail.map{lineToks => 
	  			val t = for{
	  				(variable, idx) <- variablesIndex
	  			} yield {
	  				val value = lineToks(idx)
	  				val fileName = lineToks.last
	  				Array(variable, value, fileName)
	  			}
	  			t
	  	}.flatten
  	longFormat
  }
}