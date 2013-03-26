
class MidtermPSOFitnessFunction {
  
  val coolConstant = 0.00004768372718899898
  
  def calculateFitness(p: Particle): Double = {
    var x = getX(p.x)
    var y = getY(p.x)
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

object MidFitTest extends App {
  val func = new MidtermPSOFitnessFunction
  println(func.f(-1.1101486945506736,-1.6689304516148695E-4))
}
