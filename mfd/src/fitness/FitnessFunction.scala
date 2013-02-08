package fitness

import genotype.Genotype


trait FitnessFunction {

  def calculateFitness(g : Genotype) : Double

}
