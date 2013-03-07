package fitness

import genotype.Genotype
import scala.io.Source
import collection.{SortedSet, mutable}


class MFDFitnessFunction(matrixFileName: String,
                         nSympt: Int, nDis: Int) extends FitnessFunction {

  var priorProb = Array.ofDim[Double](nDis)
  var tendencyMatrix = Array.ofDim[Double](nSympt, nDis)
  var mPlus = null.asInstanceOf[Set[Int]]
  var fitnessMap = new mutable.HashMap[Long, Double]()
  readTendencyMatrix()

  def calculateFitness(g: Genotype): Double = {
    var diagnosis = getSetIndices(g.individual, nDis)

    var l1 = L1(diagnosis)
    var l2 = L2(diagnosis)
    var l3 = L3(diagnosis)
    l1 * l2 * l3
    //    println("L1: " + l1 + "\tL2: " + l2 + "\tL3: " + l3)
  }

  def calculateFitness(diagInt: Long): Double = {
    var diagnosis = getSetIndices(diagInt, nDis)

    var l1 = L1(diagnosis)
    var l2 = L2(diagnosis)
    var l3 = L3(diagnosis)
    l1 * l2 * l3
    //    println("L1: " + l1 + "\tL2: " + l2 + "\tL3: " + l3)
  }

  def setMPlus(mPlusNum: Int) {
    mPlus = getSetIndices(mPlusNum, nSympt)
//    println(mPlusNum.toBinaryString + "\t" + mPlus)
  }

  def getSetIndices(num: Long, binStrLen: Int): Set[Int] = {
    var s = num.toBinaryString
    var res = Set[Int]()

    for (i <- 0 until s.length) {
      if (s(i) == '1') {
        res += (binStrLen - s.length + i)
      }
    }
    res
  }

  def readTendencyMatrix() {
    var lines = Source.fromFile(matrixFileName).getLines()
    for (i <- 0 until nDis) {
      priorProb(i) = lines.next().toDouble
    }

    for (i <- 0 until nSympt) {
      lines.next() //consume empty line
      for (j <- 0 until nDis) {
        tendencyMatrix(i)(j) = lines.next().toDouble
      }
    }
  }

  def L1(di: Set[Int]): Double = {
    var l1 = 1.0
    for (i <- mPlus) {
      var dProd = 1.0
      for (j <- di) {
        dProd *= (1.0 - tendencyMatrix(i)(j))
      }
      l1 *= (1.0 - dProd)
    }
    l1
  }

  def L2(di: Set[Int]): Double = {
    var absentSymptoms = (0 until nSympt).toSet &~ mPlus //correct

    var l2 = 1.0
    for (j <- di) {
      var mProd = 1.0
      for (l <- absentSymptoms) {
        if (tendencyMatrix(l)(j) > 1.0e-7) {
          mProd *= (1.0 - tendencyMatrix(l)(j))
        }
      }
      l2 *= mProd
    }
    l2
  }

  def L3(di: Set[Int]): Double = {
    var l3 = 1.0
    for (j <- di) {
      l3 *= (priorProb(j) / (1.0 - priorProb(j)))
    }
    l3
  }
}
