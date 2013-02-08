package genotype

import math.pow

class Genotype(nBits: Int, ind: Long, fit : Double = 0) {

  if (nBits > 64) {
    println("Number of bits must be less than 64")
    System.exit(-1)
  }

  val length = nBits
  var individual = ind
  var fitness = fit
//  println("nBits = "+nBits)

  /**
   * Returns the individual masked such that all bits besides those
   * from i (inclusive) to j (exclusive) become zero (zero based indexing
   * starting on the left).
   * @param i
   * @param j
   * @return
   */
  def getRange(i: Int, j: Int): Long = {
    return individual & getMask(i, j)
  }

  // If newValues has anything outside the range, it truncates it
  def setRange(i: Int, j: Int, value : Long) = {
    var mask = getMask(i, j)
    val maskedValue = value & mask
    individual &= ~mask
    individual |= maskedValue
  }

  /**
   * Generates a mask with 1's in the range from index i (inclusive) to
   * j (exclusive). (Indices count from the left and are zero-based.) If i > j,
   * then it "wraps around" such that the mask is the bitwise negation of what
   * it would be otherwise.
   * @param i
   * @param j
   * @return
   */
  def getMask(i : Int, j : Int) : Long = {
    var left = i
    var right = j
    if (i > j) {
      left = j
      right = i
    }
    var mask = (pow(2, length - left).toLong - 1) & (~(pow(2, length - right) - 1).toLong)

    if (i > j) mask = ~mask

    mask
  }

  def crossover(g2: Genotype){


  }

  override def clone() : Genotype = {
    new Genotype(length, individual, fitness)
  }

}
