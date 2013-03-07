
import util.{Random, Sorting}
import math.pow
import java.util

class TournamentSelectionEngine(k: Int, p: Double, elit: Boolean) {

  var elitism = elit
  var topInd = null.asInstanceOf[MSEGenotype]

  def select(population: Array[MSEGenotype]): Array[MSEGenotype] = {
    var nextGen = new Array[MSEGenotype](population.size)
    var rand = new Random()
    if (topInd == null || !elitism) {
      topInd = population(0)
    }
    var avFit = 0.0
    for (ind <- population) {
      avFit += ind.fitness
      if (ind.fitness > topInd.fitness)
        topInd = ind
    }
    avFit = avFit/population.size

    var counter = if (elitism) 1 else 0
    while (counter < population.size) {
      var brackSize = math.min(k, population.size - counter)
      var bracket = Array.ofDim[MSEGenotype](brackSize)
      var bracketMems = Set[MSEGenotype]()
      for (i <- 0 until brackSize) {
        var competitor = population(rand.nextInt(population.size))
        //while (bracketMems.contains(competitor)) {  
          //competitor = population(rand.nextInt(population.size))
        //}
        bracketMems += competitor
        bracket(i) = competitor
      }

      var winner = compete(bracket)
      nextGen(counter) = MSEGenotype(winner.genes.clone, winner.fitFunc) //TODO: problem? doesn't clone?
      counter += 1
    }

    if (elitism) nextGen(0) = MSEGenotype(topInd.genes.clone, topInd.fitFunc) //TODO: problem? doesn't clone?
    nextGen
  }

  def compete(b: Array[MSEGenotype]): MSEGenotype = {
    Sorting.quickSort(b)(GenotypeOrdering)
    var probs = Array.ofDim[Double](b.length)
    probs(0) = p
    for (i <- 1 until b.length) {
      probs(i) = probs(i - 1) + p * pow((1 - p), i)
    }

    var rand = new Random()
    var r = rand.nextDouble()

    var i = 0
    while (r > probs(i) && i < probs.size-1) i += 1
    b(i)
  }

  def getTopIndividual(): MSEGenotype = {
    topInd
  }

  object GenotypeOrdering extends Ordering[MSEGenotype] {
    def compare(a: MSEGenotype, b: MSEGenotype) = b.fitness compare a.fitness
  }
}
