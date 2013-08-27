package edu.yu.einstein.methylPhase

import com.beust.jcommander.{ JCommander, Parameter }
import edu.yu.einstein.methylPhase.structure.CpGRead
import edu.yu.einstein.methylPhase.io.CpGReadsReader

/**
 * For each methylation site, output the number of methylated and unmethylated reads found on each allele
 */
object SummarizePhasedCpGFile {

  object Args {
    @Parameter(names = Array("-c", "--cpg"), description = "CpG file generated with MethylPhase, containing the methylation and the allele of reads", required = true)
    var cpGReadsFilePath: String = _

    @Parameter(names = Array("-q", "--minBaseQual"), description = "Only lines where the base quality is equal to or greater than this value will be processed", required = false)
    var minBaseQual: Int = 0
  }

  /**
   * Main method that output the number of methylated and unmethylated reads found on each allele for each methylation site
   */
  def main(args: Array[String]): Unit = {
    new JCommander(Args, args.toArray: _*)
    val cpGReadReader = new CpGReadsReader(Args.cpGReadsFilePath)
    val minQual = Args.minBaseQual
    println("#chromosome\tposition\tmethylMaternal\tunmethylMaternal\tmethylPaternal\tunmethylPaternal\tambiguous\terror")
    // for each CpG read
    while (cpGReadReader.hasNext) {
      val siteCpGReads = cpGReadReader.next // CpG reads spanning the current methyl site
      val sitePos = cpGReadReader.lastMethylatedSitePosition // position of the current methyl site
      val siteChr = cpGReadReader.lastMethylatedSiteChromosome // chromosome of the current methyl site
      var errorCount = 0
      var ambiguous = 0
      var methylMaternal = 0
      var unmethylMaternal = 0
      var methylPaternal = 0
      var unmethylPaternal = 0
      val it = siteCpGReads.iterator
      while (it.hasNext) { // iterate thru reads that span the site
        val currentRead = it.next
        if (currentRead.baseQ >= minQual) {
          if (((currentRead.maternalAlleleCount > 0) && (currentRead.paternalAlleleCount > 0)) || (currentRead.errorAlleleCount > 0)) {
            errorCount += 1
          } else {
            if (currentRead.maternalAlleleCount > 0) {
              if (currentRead.isMethylated) {
                methylMaternal += 1
              } else {
                unmethylMaternal += 1
              }
            } else if (currentRead.paternalAlleleCount > 0) {
              if (currentRead.isMethylated) {
                methylPaternal += 1
              } else {
                unmethylPaternal += 1
              }
            } else if (currentRead.ambiguousAlleleCount > 0) {
              ambiguous += 1
            }
          }
        }
      }
      if ((methylMaternal != 0) || (unmethylMaternal != 0) ||
        (methylPaternal != 0) || (unmethylPaternal != 0) ||
        (ambiguous != 0) || (errorCount != 0)) {
        println(siteChr + "\t" + sitePos + "\t" +
          methylMaternal + "\t" + unmethylMaternal + "\t" +
          methylPaternal + "\t" + unmethylPaternal + "\t" +
          ambiguous + "\t" + errorCount)
      }
    }
  }
}