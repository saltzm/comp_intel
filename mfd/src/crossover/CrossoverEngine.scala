package crossover

import genotype.Genotype


trait CrossoverEngine {
  def cross( g1: Genotype, g2: Genotype)
}

