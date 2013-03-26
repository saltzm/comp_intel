
import util.{Random, Sorting}
import math.pow
import java.util

class TournamentSelectionEngine(k: Int, p: Double, elit: Boolean) {

  var elitism = elit
  var topInd = null.asInstanceOf[MidtermGenotype]

  def select(population: Array[MidtermGenotype]): Array[MidtermGenotype] = {
    var nextGen = new Array[MidtermGenotype](population.size)
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
      var bracket = Array.ofDim[MidtermGenotype](brackSize)
      var bracketMems = Set[MidtermGenotype]()
      for (i <- 0 until brackSize) {
        var competitor = population(rand.nextInt(population.size))
        //while (bracketMems.contains(competitor)) {  
          //competitor = population(rand.nextInt(population.size))
        //}
        bracketMems += competitor
        bracket(i) = competitor
      }

      var winner = compete(bracket)
      nextGen(counter) = MidtermGenotype(winner.genes.clone, winner.fitFunc) //TODO: problem? doesn't clone?
      counter += 1
    }

    if (elitism) nextGen(0) = MidtermGenotype(topInd.genes.clone, topInd.fitFunc) //TODO: problem? doesn't clone?
    println(topInd.fitness)
    nextGen
  }

  def compete(b: Array[MidtermGenotype]): MidtermGenotype = {
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

  def getTopIndividual(): MidtermGenotype = {
    topInd
  }

  object GenotypeOrdering extends Ordering[MidtermGenotype] {
    def compare(a: MidtermGenotype, b: MidtermGenotype) = b.fitness compare a.fitness
  }
}
