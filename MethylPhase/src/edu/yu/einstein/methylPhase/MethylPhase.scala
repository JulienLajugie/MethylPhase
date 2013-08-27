package edu.yu.einstein.methylPhase

import com.beust.jcommander.{ JCommander, Parameter }
import net.sf.samtools.SAMRecord
import edu.yu.einstein.methylPhase.io.{ CpGReadsReader, BAMReader, VCFReader }
import edu.yu.einstein.methylPhase.structure.{ CpGRead }

object MethylPhase {

  object Args {
    @Parameter(names = Array("-c", "--cpg"), description = "CpG file generated with Bis-SNP, containing the methylated sites", required = true)
    var cpGReadsFilePath: String = _

    @Parameter(names = Array("-b", "--bam"), description = "BAM file generated with BSMAP, containing the BS alignment", required = true)
    var bamFilePath: String = _

    @Parameter(names = Array("-v", "--vcf"), description = "VCF file containing the variants of the sample", required = true)
    var vcfFilePath: String = _

    @Parameter(names = Array("-g", "--gtInfoIndex"), description = "Index of the genotype info field of the sample to study in the VCF file", required = false)
    var vcfGenotypeInfoFieldIndex: Int = 9

    @Parameter(names = Array("-r", "--rangeAroundSite"), description = "Range to study around a methylated site (in bp).", required = false)
    var rangeAroundSite: Int = 2000
  }

  /**
   * MethylPhase main method
   */
  def main(args: Array[String]): Unit = {
    new JCommander(Args, args.toArray: _*)
    val cpGReadReader = new CpGReadsReader(Args.cpGReadsFilePath)
    val bamReader = new BAMReader(Args.bamFilePath)
    val vcfReader = new VCFReader(Args.vcfFilePath, Args.vcfGenotypeInfoFieldIndex)
    val rangeAroundSite = Args.rangeAroundSite
    println(CpGRead.toString)
    while (cpGReadReader.hasNext) { // iterate for every single methylation site
      val siteCpGReads = cpGReadReader.next // CpG reads spanning the current methyl site
      val sitePos = cpGReadReader.lastMethylatedSitePosition // position of the current methyl site
      val siteChr = cpGReadReader.lastMethylatedSiteChromosome // chromosome of the current methyl site
      val siteSurroundingsStart = Math.max(1, sitePos - rangeAroundSite)
      val siteSurroundingsStop = sitePos + rangeAroundSite
      val siteVariants = vcfReader.retrieveVariants(siteChr, siteSurroundingsStart, siteSurroundingsStop)
      if (siteVariants.isEmpty()) { // if there is no SNPs surrounding the methylation site we just print the reads
        val it = siteCpGReads.iterator
        while (it.hasNext) {
          val currentRead = it.next
          println(currentRead.toString)
        }
      } else { // case where there is variants surrounding the methylation site
        val siteBAMReads = bamReader.retrieveReads(siteChr, siteSurroundingsStart, siteSurroundingsStop)
        val it = siteCpGReads.iterator
        while (it.hasNext) {
          val currentRead = it.next
          val samMethylatedRead = siteBAMReads(currentRead.readID)
          if (samMethylatedRead != null) {
            val readBaseIterator = samMethylatedRead.iterator
            while (readBaseIterator.hasNext) {
              val samBase = readBaseIterator.next
              val variant = siteVariants.get(samBase.position)
              if (variant != null) {
                val allele = samBase.getAllele(variant)
                currentRead.addAllele(allele)
              }
            }
          }
          println(currentRead.toString)
        }
      }
    }
  }
}