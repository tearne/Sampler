package sampler.example.io

import java.nio.file.Paths
import sampler.io.CSV
import java.nio.file.Files
import java.nio.file.OpenOption
import java.nio.file.StandardOpenOption.APPEND

object CSVExample extends App{
	/*
	 * Reading CSV from class path
	 */
	val inFile = Paths.get(getClass.getClassLoader.getResource("csv/example.csv").toURI())
	
	CSV.assertHeader(inFile, "FirstCol", "SecondCol", "Third")
	val dataLines = CSV.read(inFile).drop(1)
	
	val thirdColumn = dataLines.map(_(2).toDouble).toSeq
	assert(thirdColumn == Seq(1.1, 2.2, 3.3))
	
	/*
	 * Writing CSV
	 */
	val outDir = Paths.get("results", "csv")
	Files.createDirectories(outDir)
	
	val header = Seq("A", "B", "C")
	val aData = Seq(1,2,3)
	val bData = Seq(1.1, 2.2, 3.3)
	val cData = Seq("one", "two", "three")
	
	val rowData = Seq(aData, bData, cData).transpose
	
	CSV.writeLines(outDir.resolve("myCSV.csv"), Seq(header) ++ rowData)
	
	/*
	 * Transpose whole CSV file.  Useful when it's most natural to append
	 * data columns sequentially as rows to save memory.
	 */
	val file = outDir.resolve("toTranspose.csv")
	CSV.writeLine(file, Seq("First", 1,2,3))
	CSV.writeLine(file, Seq("Second", 'a','b','c'), APPEND)
	CSV.writeLine(file, Seq("Third", "one","two","three"), APPEND)
	
	val transposed = outDir.resolve("transposeed.csv")
	CSV.transpose(file, transposed)
	
	CSV.assertHeader(transposed, "First", "Second", "Third")
	assert(CSV.read(transposed).toSeq.last == Seq("3", "c","three"))
}