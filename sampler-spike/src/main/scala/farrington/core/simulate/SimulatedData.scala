package farrington.core.simulate

import java.nio.file.Path
import java.nio.charset.Charset
import java.nio.file.Files

case class BaselineData(
    year: IndexedSeq[Int],
    month: IndexedSeq[Int],
    baseline: IndexedSeq[Int],
    mean: IndexedSeq[Double]
  )

case class OutbreakData(
    year: IndexedSeq[Int],
    month: IndexedSeq[Int],
    baseline: IndexedSeq[Int],
    counts: IndexedSeq[Int],
    hist: List[(Int, Int)],
    start: Int,
    end: Int,
    min: Int,
    max: Int
  )
case object OutbreakData {
  def writeToFile(data: OutbreakData, path: Path, filename: String) = {  
    val nData = data.baseline.length
    Files.createDirectories(path)
    val writer = Files.newBufferedWriter(path.resolve(filename), Charset.defaultCharset())
    writer.write("month, baseline, outbreak, start, end")
    writer.newLine
    writer.write(s"${1.toString}, ${data.baseline(0).toString}, ${data.counts(0).toString}, ${data.start.toString}, ${data.end.toString}")
    writer.newLine
    for (i <- 1 until nData) {
      writer.write(s"${(i+1).toString}, ${data.baseline(i).toString}, ${data.counts(i).toString}")
      writer.newLine
    }
    writer.close    
  }
}