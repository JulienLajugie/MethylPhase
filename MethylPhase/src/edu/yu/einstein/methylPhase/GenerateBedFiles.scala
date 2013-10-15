package edu.yu.einstein.methylPhase

import com.beust.jcommander.Parameter
import scala.io.Source
import java.io.PrintWriter
import com.beust.jcommander.JCommander

/**
 * Writes the result of the summarized phased CpG file into 6 bedgraph files containing the
 * results about the count reads
 * - maternal methylation
 * - maternal unmethylation
 * - paternal methylation
 * - paternal unmethylation
 * - ambiguous
 * - error
 */
object GenerateBedFiles {

  object Args {
    @Parameter(names = Array("-i", "--input"), description = "Path to the input file", required = true)
    var inputFilePath: String = _
  }

  def main(args: Array[String]): Unit = {
    new JCommander(Args, args.toArray: _*)
    val inputFileLines = Source.fromFile(Args.inputFilePath).getLines
    val prefix = Args.inputFilePath.replaceAll("\\.[^.]*$", "")
    val matMethylPrinter = new PrintWriter(prefix + "_mat_methyl.bgr")
    val matUnmethylPrinter = new PrintWriter(prefix + "_mat_unmethyl.bgr")
    val patMethylPrinter = new PrintWriter(prefix + "_pat_methyl.bgr")
    val patUnmethylPrinter = new PrintWriter(prefix + "_pat_unmethyl.bgr")
    val ambiguousPrinter = new PrintWriter(prefix + "_ambiguous.bgr")
    val errorPrinter = new PrintWriter(prefix + "_error.bgr")

    while (inputFileLines.hasNext) {
      val line = inputFileLines.next
      if (line.charAt(0) != '#') {
        val splitLine = line.split("\t")
        val chromo = splitLine(0)
        val stop = splitLine(1).toInt
        val start = stop - 1
        val lineStart = chromo + "\t" + start + "\t" + stop + "\t"
        matMethylPrinter.println(lineStart + splitLine(2))
        matUnmethylPrinter.println(lineStart + splitLine(3))
        patMethylPrinter.println(lineStart + splitLine(4))
        patUnmethylPrinter.println(lineStart + splitLine(5))
        ambiguousPrinter.println(lineStart + splitLine(6))
        errorPrinter.println(lineStart + splitLine(7))
      }
    }
    matMethylPrinter.close()
    matUnmethylPrinter.close()
    patMethylPrinter.close()
    patUnmethylPrinter.close()
    ambiguousPrinter.close()
    errorPrinter.close()
  }
}