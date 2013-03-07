package pso

import util.Random
import fitness.MFDFitnessFunction


class Particle(dims: Int, c1: Double, c2: Double, vMax: Double, w: Double, fitFunc: MFDFitnessFunction) {

  var rand = new Random()
  var x = Array.ofDim[Short](dims)

  for(i <- 0 until x.length) x(i) = rand.nextInt(2).toShort

  var fitness = calcFit
  var pbest = x.clone()
  var pbestFit = fitness

  var v = Array.ofDim[Double](dims)
  for (i <- 0 until v.length) v(i) = rand.nextDouble()*vMax

  def update(gbest: Array[Short]){
    for ( d <- 0 until dims){
      v(d) = w*v(d) + c1*rand.nextDouble()*(pbest(d)-x(d)) + 
                      c2*rand.nextDouble()*(gbest(d) - x(d))
      val vmod = 1.0/(1+math.pow(math.E, -1*v(d)))
      x(d) = if(rand.nextDouble() < vmod) 1 else 0
    }
    fitness = calcFit
    if (fitness > pbestFit){
      pbest = x.clone()
      pbestFit = fitness
    }
  }
  def calcFit: Double = {
    var partStr = ""
    x.foreach{ l => partStr+=l }
    var diagnosisLong = Integer.parseInt(partStr, 2).toLong
    fitFunc.calculateFitness(diagnosisLong)
  }
}
