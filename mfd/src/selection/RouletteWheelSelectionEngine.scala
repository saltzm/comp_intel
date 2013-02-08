package selection

import population.BitBasedPopulation
import genotype.Genotype
import util.Random

class RouletteWheelSelectionEngine(elit: Boolean = false) extends SelectionEngine {

  var elitism = elit
  var topInd = null.asInstanceOf[Genotype]

  def select(p: BitBasedPopulation) {

    var totalFitness = 0.0
    if(topInd == null || !elitism){
      topInd = p.population(0)
    }
    for (ind <- p.population) {
      var fit = ind.fitness
      totalFitness += fit
      if (fit > topInd.fitness) {
        topInd = ind
      }
    }

//    println("Top Individual: " + topInd.individual + " with fitness " + topInd.fitness)

    var roulette = new Array[(Genotype, Double)](p.size + 1)

    var idx = 0
    var prevFit = 0.0
    var normalizedFitness = 0.0
    for (ind <- p.population) {
      normalizedFitness = ind.fitness / totalFitness
      roulette(idx) = (ind, normalizedFitness + prevFit)
      prevFit += normalizedFitness
      idx += 1
    }

    var rand = new Random()
    var nextVal = rand.nextDouble()
    var selected = false
    var nextGen = new Array[Genotype](p.size)
    var startIdx = 0
    if (elitism) {
      nextGen(0) = topInd.clone()
      startIdx += 1
    }

    for (i <- startIdx until p.size) {
      for(j <- 0 until roulette.size if(!selected)){
        var (g, fit) = roulette(j)
        //println(g)
        if (nextVal < fit) {
          nextGen(i) = g.clone()
          selected = true
        }
      }
      nextVal = rand.nextDouble()
      selected = false
    }

//    println("topInd: "+topInd.individual)
    p.population = nextGen
  }

  def getTopIndividual() : Genotype = {
    topInd
  }

}
