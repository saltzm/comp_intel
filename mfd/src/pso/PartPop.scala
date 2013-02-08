package pso

import fitness.MFDFitnessFunction


class PartPop(popSize:Int, dims: Int, c1: Double, c2: Double, vMax: Double, w: Double, fitFunc: MFDFitnessFunction) {

  var pop = Array.ofDim[Particle](popSize)
  for (i <- 0 until pop.length){
    pop(i) = new Particle(dims, this, c1, c2, vMax, w)
  }

  var gbest = pop(0).x //TODO:  need to fix?  calc fitness up front?  idk
  var gbestFit = pop(0).fitness

  def calcFit(partLoc: Array[Short]): Double = {
    var partStr = ""
    partLoc.foreach{ l => partStr+=l }
    var diagLong = Integer.parseInt(partStr, 2).toLong
    fitFunc.calculateFitness(diagLong)
  }



}
