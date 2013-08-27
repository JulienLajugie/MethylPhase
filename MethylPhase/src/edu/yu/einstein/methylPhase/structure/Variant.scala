package edu.yu.einstein.methylPhase.structure

import net.sf.samtools.SAMRecord

class Variant private (val chromo: String, val pos: Int, val maternalBase: Char, val paternalBase: Char) {
}

object Variant {

  /**
   * Creates a new variant from the specified VCF line
   * The specified index is the index of the genotype info field of the sample to extract in the VCF file
   * Returns null if the variant is homozygous or if it's partially called
   */
  def createVariant(vcfLine: String, genotypeInfoFieldIndex: Int): Variant = {
    val splitLine = vcfLine.split('\t');
    if (splitLine.length < genotypeInfoFieldIndex) {
      sys.error("The specified VCF line doesn't have a valid number of fields")
      sys.error("Line: " + vcfLine)
    }
    val chromo = splitLine(0)
    val pos = splitLine(1).toInt

    val gtInfo = splitLine(genotypeInfoFieldIndex).split(':')
    val gt = gtInfo(0).split('|')
    if (gt.length != 2) {
      return null
    }
    val maternalAllele = gt(0)
    val paternalAllele = gt(1)

    if (paternalAllele == '?' || maternalAllele == '?' || paternalAllele == maternalAllele) {
      return null
    }

    //println(maternalAllele);
    val maternalBase = maternalAllele match {
      case "0" => splitLine(3).charAt(0)
      case "1" => splitLine(4).charAt(0)
      case _ => '?'
    }
    val paternalBase = paternalAllele match {
      case "0" => splitLine(3).charAt(0)
      case "1" => splitLine(4).charAt(0)
      case _ => '?'
    }

    new Variant(chromo, pos, maternalBase, paternalBase)
  }
}