package edu.yu.einstein.methylPhase.structure

/**
 * Enumeration of the different alleles.
 * Two special values were added:
 * - ambiguous for the cases where the allele cannot be found
 * - error for the cases where there is no valid allele
 */
object Allele extends Enumeration {

  type Allele = Value

  val Paternal, Maternal, Ambiguous, Error = Value

}

