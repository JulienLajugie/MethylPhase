package edu.yu.einstein.methylPhase.structure

import edu.yu.einstein.methylPhase.structure.Allele._

/**
 * Represents a base from a SAM record
 */
class SAMBase(val position: Int, val value: Char) {

  def getAllele(variant: Variant): Allele = {
    if (canBeOnSameAlleleAs(variant.maternalBase)) {
      if (canBeOnSameAlleleAs(variant.paternalBase)) {
        // case where the base can be from the maternal or the paternal allele
        Allele.Ambiguous
      } else {
        // case where base is from the maternal allele
        Allele.Maternal
      }
    } else if (canBeOnSameAlleleAs(variant.paternalBase)) {
      // case where the base is from the paternal allele
      Allele.Paternal
    } else {
      // case where the base is neither from the paternal nor from the maternal allele -> error
      Allele.Error
    }
  }

  /**
   * @return true if this base is possibly on same allele as the specified base from a variant
   */
  private def canBeOnSameAlleleAs(variantBase: Char): Boolean = {
    if (variantBase == value) {
      true
    } else {
      variantBase match {
        case 'A' => value == 'R'
        case 'C' => value == 'Y'
        case 'G' => value == 'R'
        case 'T' => value == 'Y'
        case 'N' => true
        case _ => sys.error("Invalid base")
      }
    }
  }
}