package edu.yu.einstein.methylPhase.io

import java.io.File
import net.sf.samtools.SAMFileReader
import edu.yu.einstein.methylPhase.structure.SAMReadDoubleMap
import edu.yu.einstein.methylPhase.structure.SAMReadDoubleMap

class BAMReader(val bamFilePath: String) {

  val samFileReader = new SAMFileReader(new File(bamFilePath))
  private val sortedReadMap = new SAMReadDoubleMap
  private var mappedChromo: String = _

  def retrieveReads(chromo: String, start: Int, stop: Int): SAMReadDoubleMap = {
    if (mappedChromo == null) {
      // case where we start the 1st chromosome
      mappedChromo = chromo
    } else if (mappedChromo != chromo) {
      // case where we start a new chromosome
      mappedChromo = chromo
      sortedReadMap.clear()
    } else {
      // case where we are still on the same chromosome as last time
      sortedReadMap.removeReadBeforePosition(start)
    }
    val smallerVarToRetrieve = if (sortedReadMap.lastPosition == -1) start else sortedReadMap.lastPosition
    addReads(chromo, smallerVarToRetrieve, stop)
    sortedReadMap
  }

  /**
   * Adds the reads in the specified interval to the map
   */
  private def addReads(chromo: String, start: Int, stop: Int) {
    val spanningReadIterator = samFileReader.queryOverlapping(chromo, start, stop)
    while (spanningReadIterator.hasNext()) {
      sortedReadMap.addRecord(spanningReadIterator.next())
    }
    spanningReadIterator.close()
  }
}
