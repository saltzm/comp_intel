package fitness

import genotype.Genotype


class SineFunction extends FitnessFunction{

  def calculateFitness(g:Genotype) : Double = {
    math.sin(math.Pi*g.individual/256)
  }
}
