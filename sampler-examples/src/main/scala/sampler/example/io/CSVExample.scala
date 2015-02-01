package sampler.example.io

import java.nio.file.Paths
import sampler.io.CSV
import java.nio.file.Files

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
}