package edu.yu.einstein.methylPhase.io

import scala.io._
import edu.yu.einstein.methylPhase.structure.CpGRead
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Iterable

/**
 * Reader that reads CpGreads bed files generated by BisSNP
 *
 * @constructor create a new CpGReadsReader for the specified file
 * @param cpGReadFilePath path to a CpGReads file
 */
class CpGReadsReader(cpGReadFilePath: String) extends Iterator[Seq[CpGRead]] {

  private val fileLines = Source.fromFile(cpGReadFilePath).getLines
  private var _lastMethylSitePos = 0
  private var _lastMethylSiteChr: String = _
  private var firstReadOfSite: CpGRead = _
  private var readList: Seq[CpGRead] = _

  def lastMethylatedSitePosition(): Int = _lastMethylSitePos
  def lastMethylatedSiteChromosome(): String = _lastMethylSiteChr

  override def hasNext(): Boolean = {
    readList = extractMethylSite
    return readList != null
  }

  private def extractMethylSite = {
    val readList = new ArrayBuffer[CpGRead]()
    var line: String = null;
    var isSiteExtractionDone = false

    while (!isSiteExtractionDone && fileLines.hasNext) {
      line = fileLines.next
      if (line.charAt(0) != '#') {
        var extractedRead = CpGRead.createCPGRead(line)
        if (readList.isEmpty) {
          if (firstReadOfSite != null) {
            readList += firstReadOfSite
          }
          _lastMethylSitePos = extractedRead.position
          _lastMethylSiteChr = extractedRead.chromosome
          readList += extractedRead
        } else if (_lastMethylSitePos != extractedRead.position) {
          firstReadOfSite = extractedRead
          isSiteExtractionDone = true
        } else {
          readList += extractedRead
        }
      }
    }
    // return null if the list of reads is empty
    if (readList.isEmpty) {
      null
    } else {
      readList
    }
  }

  /**
   * @return a list containing all the {@link CpGRead} objects that span the next methylated site.
   * Returns null when there is no more site to extract
   */
  override def next(): Seq[CpGRead] = readList
}