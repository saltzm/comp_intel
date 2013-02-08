package crossover

import util.Random
import genotype.Genotype

class OnePointCrossoverEngine(prob : Double = 0.4) extends CrossoverEngine{

  def cross( g1: Genotype, g2: Genotype) = {
    val rand = new Random()
    if (rand.nextDouble() < prob){
      val genotypeLength = g1.length
      val crossoverPoint = rand.nextInt(genotypeLength+1)

      val g1Genes = g1.individual
      g1.setRange(crossoverPoint, genotypeLength, g2.individual)
      g2.setRange(crossoverPoint, genotypeLength, g1Genes)
    }
  }

}
