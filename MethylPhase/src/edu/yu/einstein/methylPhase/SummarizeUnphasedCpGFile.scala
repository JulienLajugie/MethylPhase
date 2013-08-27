package edu.yu.einstein.methylPhase

import com.beust.jcommander.{ JCommander, Parameter }
import edu.yu.einstein.methylPhase.structure.CpGRead
import edu.yu.einstein.methylPhase.io.CpGReadsReader

/**
 * For each methylation site, output the number of methylated and unmethylated reads found.
 */
object SummarizeUnphasedCpGFile {

  object Args {
    @Parameter(names = Array("-c", "--cpg"), description = "CpG file generated with MethylPhase, containing the methylation and the allele of reads", required = true)
    var cpGReadsFilePath: String = _

    @Parameter(names = Array("-q", "--minBaseQual"), description = "Only lines where the base quality is equal to or greater than this value will be processed", required = false)
    var minBaseQual: Int = 0

    @Parameter(names = Array("-u", "--umethylated"), description = "If this flag is set the summary will contain the unmethylated read counts instead of methylated read counts", required = false)
    var summarizeUnmethylated: Boolean = false
  }

  /**
   * Main method that output the number of methylated and unmethylated reads found on each allele for each methylation site
   */
  def main(args: Array[String]): Unit = {
    new JCommander(Args, args.toArray: _*)
    val cpGReadReader = new CpGReadsReader(Args.cpGReadsFilePath)
    val minQual = Args.minBaseQual
    val scoreStr = if (Args.summarizeUnmethylated) "unmethylReadCount" else "methylReadCount"
    println("#chromosome\tstart\tstop\t" + scoreStr)
    // for each CpG read
    while (cpGReadReader.hasNext) {
      val siteCpGReads = cpGReadReader.next // CpG reads spanning the current methyl site
      val sitePos = cpGReadReader.lastMethylatedSitePosition // position of the current methyl site
      val siteChr = cpGReadReader.lastMethylatedSiteChromosome // chromosome of the current methyl site
      var score = 0
      val it = siteCpGReads.iterator
      while (it.hasNext) { // iterate thru reads that span the site
        val currentRead = it.next
        if (currentRead.baseQ >= minQual) {
          if (currentRead.isMethylated && !Args.summarizeUnmethylated) {
            score += 1
          } else if (!currentRead.isMethylated && Args.summarizeUnmethylated) {
            score += 1
          }
        }
      }
      if (score > 0) {
        println(siteChr + "\t" + (sitePos - 1) + "\t" + sitePos + "\t" + score)
      }
    }
  }
}