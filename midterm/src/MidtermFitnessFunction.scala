
class MidtermFitnessFunction {
  
  val coolConstant = 0.00004768372718899898
  
  def calculateFitness(p: MidtermGenotype): Double = {
    var x = getX(p.genes)
    var y = getY(p.genes)
    f(x,y)
  }

  def f(x: Double, y: Double): Double = {
    val sumOfSquares = x*x + y*y
    val sqrtOfSquares = math.sqrt(sumOfSquares)
    val sinOfSqrt = math.sin(sqrtOfSquares)
    val sinSquared = math.pow(sinOfSqrt, 2)
    val denom = math.pow(1.0 + 0.001 * sumOfSquares, 2)
    0.5 + (sinSquared - 0.5)/denom
  }

  def getX(xArray: Array[Int]): Double = {
    var xString = ""
    for (i <- 0 until 32) xString += xArray(i) 
    java.lang.Long.parseLong(xString, 2).toInt * coolConstant - 100.0
  }

  def getY(yArray: Array[Int]): Double = {
    var yString = ""
    for (i <- 33 until 64) yString += yArray(i) 
    java.lang.Long.parseLong(yString, 2).toInt * coolConstant - 100.0
  }
}

