package edu.yu.einstein.methylPhase.structure

import scala.collection.mutable.{ HashMap, ArrayBuffer }
import java.util.TreeMap
import net.sf.samtools.SAMRecord
import jonelo.jacksum.algorithm.Crc64
import jonelo.jacksum.util.Service

/**
 * Map of SAMMethylatedReadPair with 2 indexes:
 * - a position index
 * - a encrypted read name index
 */
class SAMReadDoubleMap {

  private val positionMap = new TreeMap[Int, ArrayBuffer[SAMMethylatedReadPair]]
  private val encriptedReadNameMap = new HashMap[String, SAMMethylatedReadPair]

  /**
   * Adds a SAMRecord to the double map
   */
  def addRecord(samRecord: SAMRecord) {
    def encriptReadName(readName: String): String = {
      val crc = new Crc64
      crc.update(readName.getBytes());
      Service.format(crc.getByteArray());
    }
    val position = samRecord.getAlignmentStart()
    val encriptedReadName = encriptReadName(samRecord.getReadName())
    if (encriptedReadNameMap.contains(encriptedReadName)) {
      encriptedReadNameMap(encriptedReadName).addMate(samRecord)
    } else {
      val readPair = SAMMethylatedReadPair.createInstance(samRecord, encriptedReadName)
      if (readPair != null) {
        encriptedReadNameMap += encriptedReadName -> readPair
        if (positionMap.containsKey(position)) {
          positionMap.get(position) += readPair
        } else {
          val readList = new ArrayBuffer[SAMMethylatedReadPair]
          readList += readPair
          positionMap.put(position, readList)
        }
      }
    }
  }

  /**
   * Overload parenthesis with an encrypted name as parameter.
   * @return a SAMMethylatedReadPair with the specified encrypted name
   */
  def apply(encriptedReadName: String): SAMMethylatedReadPair = {
    if (encriptedReadNameMap.contains(encriptedReadName))
      encriptedReadNameMap(encriptedReadName)
    else
      null
  }

  /**
   * Clears the map
   */
  def clear() = positionMap.clear(); encriptedReadNameMap.clear

  /**
   * @return the last position in the map or -1 if the map is empty.
   */
  def lastPosition(): Int = if (positionMap.isEmpty()) -1 else positionMap.lastKey()

  /**
   * Removes the read located before the specified position
   */
  def removeReadBeforePosition(position: Int) {
    while (!positionMap.isEmpty() && positionMap.firstKey() < position) {
      val readsToRemove = positionMap.pollFirstEntry()
      for (readToRemove <- readsToRemove.getValue()) {
        encriptedReadNameMap -= readToRemove.encriptedName
      }
      // optional: clear the array buffer to avoid memory leaks
      readsToRemove.getValue().clear
    }
  }
}