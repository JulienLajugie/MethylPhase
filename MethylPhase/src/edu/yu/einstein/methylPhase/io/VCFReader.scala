package edu.yu.einstein.methylPhase.io

import net.sf.jannot.tabix.TabixReader
import net.sf.jannot.source.Locator
import edu.yu.einstein.methylPhase.structure.Variant
import java.util.SortedMap
import java.util.TreeMap

/**
 * Creates an instnace
 * @constructor creates an instance
 * @param vcfFilePath path to the VCF file
 * @param genotypeInfoFieldIndex index of the genotype info field of the sample to study in the VCF file
 */
class VCFReader(vcfFilePath: String, genotypeInfoFieldIndex: Int) {

  private val locator = new Locator(vcfFilePath)
  private val vcfFile = new TabixReader(locator)
  private val sortedVariantMap = new TreeMap[Int, Variant]
  private var mappedChromo: String = _

  def retrieveVariants(chromo: String, start: Int, stop: Int): SortedMap[Int, Variant] = {
    if (mappedChromo == null) {
      // case where we start the 1st chromosome
      mappedChromo = chromo
    } else if (mappedChromo != chromo) {
      // case where we start a new chromosome
      mappedChromo = chromo
      sortedVariantMap.clear()
    } else {
      // case where we are still on the same chromosome as last time
      removeElementsSmallerThan(start)
    }
    val smallerVarToRetrieve = {
      if (!sortedVariantMap.isEmpty()) {
        sortedVariantMap.lastKey()
      } else {
        start
      }
    }
    addVariants(chromo, smallerVarToRetrieve, stop)
    sortedVariantMap
  }

  private def addVariants(chromo: String, start: Int, stop: Int) {
    val it = vcfFile.query(chromo + ":" + start + "-" + stop)
    if (it != null) {
      var line = it.next()
      while (line != null) {
        val variant = Variant.createVariant(line, genotypeInfoFieldIndex)
        if (variant != null) {
          sortedVariantMap.put(variant.pos, variant)
        }
        line = it.next()
      }
    }
  }

  private def removeElementsSmallerThan(start: Int) = {
    while (!sortedVariantMap.isEmpty() && sortedVariantMap.firstKey() < start) {
      sortedVariantMap.pollFirstEntry()
    }
  }
}

