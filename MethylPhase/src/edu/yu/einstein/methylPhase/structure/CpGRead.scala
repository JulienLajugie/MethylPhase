package edu.yu.einstein.methylPhase.structure

import edu.yu.einstein.methylPhase.structure.Strand._
import edu.yu.einstein.methylPhase.structure.Allele._

/**
 * A CPG read instance represents data from a line of a CpGRead bed file from BisSNP
 *
 * @constructor create a new CpGRead with the specified values
 * @param chromosome chromosome of the read
 * @param position position of the methylation site
 * @param isMethylated true if the site is methylated in the read
 * @param baseQ quality of the base of the read located on the methylation site
 * @param strand strand of the read
 * @param readID read id
 */
class CpGRead private (
  val chromosome: String,
  val position: Int,
  val isMethylated: Boolean,
  val baseQ: Int,
  val strand: Strand,
  val readID: String) {

  var maternalAlleleCount = 0
  var paternalAlleleCount = 0
  var errorAlleleCount = 0
  var ambiguousAlleleCount = 0

  /**
   *  Adds an allele to the counters of the CpG read
   */
  def addAllele(allele: Allele) = {
    allele match {
      case Maternal => maternalAlleleCount += 1
      case Paternal => paternalAlleleCount += 1
      case Error => errorAlleleCount += 1
      case Ambiguous => ambiguousAlleleCount += 1
    }
  }

  override def toString(): String = {
    chromosome + "\t" +
      position + "\t" +
      { if (isMethylated) "m" else "u" } + "\t" +
      baseQ + "\t" +
      strand + "\t" +
      readID + "\t" +
      maternalAlleleCount + "\t" +
      paternalAlleleCount + "\t" +
      errorAlleleCount + "\t" +
      ambiguousAlleleCount
  }
}

/**
 * Factory object for the CpGRead class
 */
object CpGRead {

  /**
   * Creates an instance of [[CpGRead]]
   * @param chromosome chromosome of the read
   * @param position position of the methylation site
   * @param isMethylated true if the site is methylated in the read
   * @param baseQ quality of the base of the read located on the methylation site
   * @param strand strand of the read
   * @param readID read id
   */
  def createCPGRead(cPGReadLine: String): CpGRead = {
    val splitLine = cPGReadLine.split('\t');
    if ((splitLine.length != 6) && (splitLine.length != 10)) {
      sys.error("The specified line doesn't have a valid number of fields")
      sys.error("Line: " + cPGReadLine)
    }
    val chromosome = splitLine(0)
    val position = splitLine(1).toInt
    val isMethylated = splitLine(2).trim().charAt(0) == 'm'
    val baseQ = splitLine(3).toInt
    val strand = Strand.withName(splitLine(4).trim())
    val readID = splitLine(5);
    val cpgRead = new CpGRead(chromosome, position, isMethylated, baseQ, strand, readID);
    if (splitLine.length == 10) {
      cpgRead.maternalAlleleCount = splitLine(6).toInt
      cpgRead.paternalAlleleCount = splitLine(7).toInt
      cpgRead.errorAlleleCount = splitLine(8).toInt
      cpgRead.ambiguousAlleleCount = splitLine(9).toInt
    }
    cpgRead
  }

  override def toString(): String = {
    "#chromosome\t" +
      "position\t" +
      "methylation\t" +
      "baseQ\t" +
      "strand\t" +
      "readID\t" +
      "maternalAlleleCount\t" +
      "paternalAlleleCount\t" +
      "errorAlleleCount\t" +
      "ambiguousAlleleCount\t"
  }
}