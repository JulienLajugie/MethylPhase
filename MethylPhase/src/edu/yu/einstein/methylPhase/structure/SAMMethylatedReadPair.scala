package edu.yu.einstein.methylPhase.structure

import net.sf.samtools.SAMRecord
import net.sf.samtools.SAMFileReader

/**
 * SAM read wrapper that adds operations to study the methylation of the read.
 * Uses the SAM file generated with BSMAP software
 */
class SAMMethylatedReadPair private (private val samRecord: SAMRecord, val encriptedName: String) extends Iterable[SAMBase] {

  /**
   * mate of the read specified during construction
   */
  private var _mateRecord: SAMRecord = null

  /**
   * @return the first read of the pair of reads (null if not set)
   */
  def firstOfPair = if (samRecord.getFirstOfPairFlag()) samRecord else _mateRecord

  /**
   * @return the second read of the pair of reads (null if not set)
   */
  def secondOfPair = if (!samRecord.getFirstOfPairFlag()) samRecord else _mateRecord

  /**
   * String of the sequence of the read of the pair before BS
   * Y stands for pYrimidine (C or T)
   * R stands for puRine (G or A)
   */
  private lazy val firstOfPairSequence = computeOriginalSequence(firstOfPair)

  /**
   * String of the sequence of the first read of the pair before BS
   * Y stands for pYrimidine (C or T)
   * R stands for puRine (G or A)
   */
  private lazy val secondOfPairSequence = computeOriginalSequence(secondOfPair)

  /**
   * @return a String representing the sequence of the read before BS process.
   * T and A bases are ambiguous
   */
  private def computeOriginalSequence(samRecord: SAMRecord): String = {
    if (samRecord == null) {
      null
    } else {
      val strand = samRecord.getStringAttribute("ZS")
      var readSequence = samRecord.getReadString()
      strand match {
        case "++" | "+-" => readSequence.replace('T', 'Y') // T can actually be unmethyl  C or T (pyrimidine)
        case "-+" | "--" => readSequence.replace('A', 'R') // A can actually be G (reverse complement of unmethyl C) or A (purine)
      }
    }
  }

  /**
   * Adds the second element of the pair if it has not previously been added
   */
  def addMate(mateRecord: SAMRecord) {
    if ((_mateRecord == null)
      && (!mateRecord.equals(samRecord))
      && SAMMethylatedReadPair.isValid(mateRecord)
      && SAMMethylatedReadPair.hasValidPair(samRecord)) {
      _mateRecord = mateRecord
    }
  }

  /**
   * @return an iterator to iterate on every base of the pair of reads
   */
  override def iterator() = {
    new ReadPairBasesIterator
  }

  /**
   * Iterator that iterates on the bases of the 2 pairs of a SAMMethylatedReadPair
   */
  class ReadPairBasesIterator extends Iterator[SAMBase] {

    /**
     * The current read of the pair
     */
    private var iteratorPair = if (firstOfPair != null) firstOfPair else secondOfPair

    /**
     * Index of the next alignment block that the iterator will return
     */
    private var iteratorBlockIndex = 0

    /**
     * Index of the next base within the current alignment block the iterator will return
     */
    private var iteratorIndexWithinBlock = 0

    override def hasNext(): Boolean = iteratorBlockIndex < iteratorPair.getAlignmentBlocks().size()

    override def next(): SAMBase = {
      def iteratedPairSequence(): String = if (iteratorPair == firstOfPair) firstOfPairSequence else secondOfPairSequence
      val baseIndex = iteratorPair.getAlignmentBlocks().get(iteratorBlockIndex).getReadStart() + iteratorIndexWithinBlock - 1
      val baseChar = iteratedPairSequence.charAt(baseIndex)
      val basePos = iteratorPair.getAlignmentBlocks().get(iteratorBlockIndex).getReferenceStart() + iteratorIndexWithinBlock

      // increments the iterator
      iteratorIndexWithinBlock += 1
      if (iteratorIndexWithinBlock >= iteratorPair.getAlignmentBlocks().get(iteratorBlockIndex).getLength()) {
        iteratorIndexWithinBlock = 0
        iteratorBlockIndex += 1
      }
      if ((iteratorBlockIndex >= iteratorPair.getAlignmentBlocks().size()) && (iteratorPair == firstOfPair) && (secondOfPair != null)) {
        iteratorBlockIndex = 0
        iteratorPair = secondOfPair
      }
      new SAMBase(basePos, baseChar)
    }
  }
}

/**
 * Factory for SAMMethylatedReadPair instance
 */
object SAMMethylatedReadPair {

  /**
   * @return a new instance of SAMMethylatedReadPair if the specified record is valid.
   * Null otherwise.
   */
  def createInstance(samRecord: SAMRecord, encriptedName: String): SAMMethylatedReadPair = {
    if (isValid(samRecord))
      new SAMMethylatedReadPair(samRecord, encriptedName)
    else
      null
  }

  /**
   * @param SAMRecord a SAM record
   * @return true if the specified record is valid, false otherwise
   */
  private def isValid(samRecord: SAMRecord): Boolean = {
    // no valid if it's a PCR or optical duplicate
    if (samRecord.getDuplicateReadFlag()) {
      false
    } else if (samRecord.getReadFailsVendorQualityCheckFlag()) {
      // not valid if fails to meet vendor quality requirements
      false
      /*} else if (samRecord.isValid() != null) {
      // not valid if doesn't pass SAMTools validation tests
      false*/
    } else {
      // valid
      true
    }
  }

  /**
   * @param SAMRecord a SAM record
   * @return true if the specified SAM record has a mate that is properly paired
   */
  private def hasValidPair(samRecord: SAMRecord): Boolean = {
    if (samRecord.getMateUnmappedFlag()) {
      false
    } else if (!samRecord.getProperPairFlag()) {
      false
    } else {
      true
    }
  }
}
