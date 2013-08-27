package edu.yu.einstein.methylPhase.structure

import java.util.NoSuchElementException

/**
 * Enumeration of the different Strands: + strand (or 5' strand) and - strand (or 3' strand)
 *
 */
object Strand extends Enumeration {
  type Strand = Value
  val Plus = Value("+")
  val Minus = Value("-")

  /*def lookup(symbol: Char): Strand = {
    if (symbol == '+') {
      Strand.Plus
    } else if (symbol == '-') {
      Strand.Minus
    } else {
      throw new NoSuchElementException
    }
  }*/

  override def toString(): String = {
    Value match {
      case Plus => "+"
      case Minus => "-"
    }
  }
}