package edu.yu.einstein.methylPhase

import com.beust.jcommander.Parameter
import edu.yu.einstein.methylPhase.io.BAMReader
import com.beust.jcommander.JCommander

/**
 * Retrieve a read from a specified BAM file and print its information
 */
object RetrieveRead {

  object Args {
    @Parameter(names = Array("-b", "--bam"), description = "BAM file containing the read", required = true)
    var bamFilePath: String = _

    @Parameter(names = Array("-c", "--chromosome"), description = "Chromosome of the read", required = true)
    var chromosome: String = _

    @Parameter(names = Array("-p", "--position"), description = "Position of the read", required = true)
    var position: Int = _

    @Parameter(names = Array("-n", "--name"), description = "Name of the read", required = true)
    var readName: String = _
  }

  /**
   * RetrieveRead main method
   */
  def main(args: Array[String]): Unit = {
    new JCommander(Args, args.toArray: _*)
    val bamReader = new BAMReader(Args.bamFilePath)
    val chromosome = Args.chromosome
    val position = Args.position
    val readName = Args.readName
    val siteBAMReads = bamReader.retrieveReads(chromosome, position - 4000, position + 4000)
    if (siteBAMReads(readName).firstOfPair != null) {
      println("First of pair:")
      println(siteBAMReads(readName).firstOfPair.getSAMString())
    }
    if (siteBAMReads(readName).secondOfPair != null) {
      println("Second of pair:")
      println(siteBAMReads(readName).secondOfPair.getSAMString())
    }
  }
}