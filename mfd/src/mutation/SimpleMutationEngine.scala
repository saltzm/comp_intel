package mutation

import genotype.Genotype
import util.Random
import math.pow

class SimpleMutationEngine(mutationProb: Double) extends MutationEngine {

  var mProb = mutationProb

  def mutate(g : Genotype){
    if (mProb < 0){
      println("Mutation probability must be between 0 and 1.")
      System.exit(-1)
    }

    var mutateMask = 0.toLong
    val rand = new Random()

    for( i <- 0 until g.length){
      if (rand.nextDouble() < mProb){
        mutateMask += pow(2, i).toLong
      }
    }
    var mutation = g.individual ^ mutateMask
    g.individual = mutation

  }
  def incMutProb(m: Double){
    mProb += m
  }

}
