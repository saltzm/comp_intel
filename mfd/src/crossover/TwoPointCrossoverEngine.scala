package crossover

import genotype.Genotype
import util.Random

class TwoPointCrossoverEngine(prob: Double = 0.4) extends CrossoverEngine{

  def cross( g1: Genotype, g2: Genotype) = {
    val rand = new Random()
    if ( rand.nextDouble() < prob ){
      val genotypeLength = g1.length
      var cPoint1 = rand.nextInt(genotypeLength+1)
      var cPoint2 = rand.nextInt(genotypeLength+1)

      var g1Genes = g1.individual
      g1.setRange(cPoint1, cPoint2, g2.individual)
      g2.setRange(cPoint1, cPoint2, g1Genes)
    }
  }

}
