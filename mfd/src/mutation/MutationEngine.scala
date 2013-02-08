package mutation

import genotype.Genotype

trait MutationEngine {
  def mutate(g : Genotype)
  def incMutProb(m: Double)
}
