package pso

import fitness.MFDFitnessFunction

class PartPop(popSize:Int, dims: Int, c1: Double, c2: Double, vMax: Double, w: Double, fitFunc: MFDFitnessFunction) {

  var pop = Array.ofDim[Particle](popSize)
  pop(0) = new Particle(dims, c1, c2, vMax, w, fitFunc)
  var gbest = pop(0).x
  var gbestFit = pop(0).fitness

  for (i <- 1 until pop.length){
    pop(i) = new Particle(dims, c1, c2, vMax, w, fitFunc)
    if (pop(i).fitness > gbestFit){
      gbest = pop(i).x
      gbestFit = pop(i).fitness
    }
  }
}
