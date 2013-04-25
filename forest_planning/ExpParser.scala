import scala.io.Source

object Parser extends App {
  var count = 0
  var sum = 0.0
  var max = Double.MinValue
  var min = Double.MaxValue
  for (line <- Source.stdin.getLines){
    val temp = line.split(" ")(2).toDouble
    sum += temp
    if (temp > max) max = temp
    else if (temp < min) min = temp
    count += 1
  }
  println("Average: " + sum/count.toDouble)
  println("Min: " + min)
  println("Max: " + max)
}
