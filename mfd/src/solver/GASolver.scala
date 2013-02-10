package solver

import population.BitBasedPopulation
import fitness.{FitnessFunction}
import selection.{SelectionEngine}
import crossover.{CrossoverEngine}
import mutation.{MutationEngine}

class GASolver(pSize: Int, gLength: Int, fitFunc: FitnessFunction, selectEng: SelectionEngine,
               crossEng: CrossoverEngine, mutEng: MutationEngine, verbose: Boolean = false) {

  var pop = null.asInstanceOf[BitBasedPopulation]
  var fitnessFunc = fitFunc
  var selector = selectEng
  var crossEngine = crossEng
  var mutationEngine = mutEng
  var popSize = pSize
  var genotypeLength = gLength

 //   val out = new java.io.FileWriter("genStuff"+selectEng+".txt")

  def solve(nGens: Int) {
    pop = new BitBasedPopulation(popSize, genotypeLength)
    calculatePopulationFitness()
    for (i <- 0 until nGens) {
      selector.select(pop)                           //creates new population
  //  if(i%5==0) out.write(i+"\t"+getSolutionFitness())
      performOperationsOnPopulation()                //performs mutation and crossover on new population
      calculatePopulationFitness()                   //recalculates their fitness values
    }

    if (verbose)
      println("Top individual: "+selector.getTopIndividual().individual+" with fitness "+selector.getTopIndividual().fitness)
   //out.close() 
  }

  def getSolutionFitness(): Double = {
    selector.getTopIndividual().fitness
  }

  def calculatePopulationFitness(){
    for (ind <- pop.population) {
      ind.fitness = fitnessFunc.calculateFitness(ind)
    }
  }

  def performOperationsOnPopulation(){
    for (i <- 0 until pop.size) {
      if (i % 2 == 0 && i != pop.size - 1) {
        var g1 = pop.member(i)
        var g2 = pop.member(i + 1)
        crossEngine.cross(g1, g2)
        mutationEngine.mutate(g1)
        mutationEngine.mutate(g2)
      }
    }
  }
}
