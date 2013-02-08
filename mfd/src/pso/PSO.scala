package pso

import fitness.MFDFitnessFunction
import io.Source


object PSO extends App {

  var fitnessFunc = new MFDFitnessFunction("TendencyMatrix10x25.txt", 10, 25)
  var lines = Source.fromFile("ExhaustiveResults10x25.txt").getLines()
  var exResults = Array.ofDim[Double](1024, 3)

  for (i <- 1 until 1024) {
    var parts = lines.next().trim.split( """\s+""")
    var first = parts(1)
    var second = parts(2)
    var third = parts(3)
    exResults(i)(0) = first.toDouble
    exResults(i)(1) = second.toDouble
    exResults(i)(2) = third.toDouble
  }

  var nGens = 50
  var noReps = 1
  var popSize = 100
  var start = 1
  var finish = 1024


  var noFirst = 0
  for (i <- 0 until noReps) {
    for (sympSet <- 1 until 1024) {
      println(sympSet)
      fitnessFunc.setMPlus(sympSet)

      var pop = new PartPop(popSize, 25, 2, 2, 4, 1, fitnessFunc)
      for (i <- 0 until nGens) {
        for (part <- pop.pop) {
          part.update()
        }
      }
      if (math.abs(pop.gbestFit - exResults(sympSet)(0)) < 1e-7) noFirst += 1
    }
  }

  println(noFirst.toDouble/(noReps*finish))

}
