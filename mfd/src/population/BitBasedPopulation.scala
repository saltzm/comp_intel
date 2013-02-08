package population

import genotype.Genotype
import util.Random
import math.pow

class BitBasedPopulation(popSize: Int, genotypeLength: Int) extends Population{

  var population = new Array[Genotype](popSize)
  var rand = new Random()
  var size = popSize

  for(i <- 0 until size){
    population(i) = new Genotype(genotypeLength, rand.nextInt(pow(2, genotypeLength).toInt))
  }

  def member(i: Int) : Genotype = {
    population(i)
  }
}
