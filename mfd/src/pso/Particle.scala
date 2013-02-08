package pso

import util.Random

class Particle(dims: Int, parentPop: PartPop, c1: Double, c2: Double, vMax: Double, w: Double) {
  var pop = parentPop

  var rand = new Random()
  var x = Array.ofDim[Short](dims)

  for(i <- 0 until x.length){
    x(i) = rand.nextInt(2).toShort
  }

  var fitness = pop.calcFit(x)

  var pbest = x.clone()
  var pbestFit = fitness


  var v = Array.ofDim[Double](dims)
  for (i <- 0 until v.length){
    v(i) = rand.nextDouble()*vMax
  }

  def update(){
    for ( d <- 0 until dims){
      v(d) = w*v(d) + c1*rand.nextDouble()*(pbest(d)-x(d)) + c2*rand.nextDouble()*(pop.gbest(d) - x(d))
      val vmod = 1.0/(1+math.pow(math.E, -1*v(d)))
      x(d) = if(rand.nextDouble() < vmod) 1 else 0
    }
    fitness = pop.calcFit(x)
    if (fitness > pbestFit){
      pbest = x.clone()
      pbestFit = fitness
      if (fitness > pop.gbestFit){
        pop.gbest = x.clone()
        pop.gbestFit = fitness
      }
    }
  }
}
