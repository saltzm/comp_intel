package selection

import population.BitBasedPopulation
import genotype.Genotype
import util.{Random, Sorting}
import math.pow
import java.util

class TournamentSelectionEngine(k: Int, p: Double, elit: Boolean) extends SelectionEngine {

  var elitism = elit
  var topInd = null.asInstanceOf[Genotype]

  def select(population: BitBasedPopulation) {
    var nextGen = new Array[Genotype](population.size)
    var rand = new Random()

    if (topInd == null || !elitism) {
      topInd = population.member(0)
    }
//    println("----------------")
    var avFit = 0.0
    for (ind <- population.population) {
//      println(ind.individual+"\t"+ind.fitness)
      avFit += ind.fitness
      if (ind.fitness > topInd.fitness)
        topInd = ind
    }
    avFit = avFit/population.size
//    println("avFit: "+avFit)

    //var counter = 0
    var counter = if (elitism) 1 else 0
    while (counter < population.size) {
      var brackSize = math.min(k, population.size - counter)
      var bracket = Array.ofDim[Genotype](brackSize)
      var bracketMems = Set[Genotype]()
      for (i <- 0 until brackSize) {
        var competitor = population.member(rand.nextInt(population.size))
        while (bracketMems.contains(competitor))
          competitor = population.member(rand.nextInt(population.size))
        bracketMems += competitor
        bracket(i) = competitor
//        println("counter: "+counter)
      }
//      print(counter+" ")

      var winner = compete(bracket)
      nextGen(counter) = winner.clone()
      counter += 1
    }

    if (elitism) nextGen(0) = topInd.clone()
    population.population = nextGen
//    println("topInd: "+topInd.individual)
  }

  def compete(b: Array[Genotype]): Genotype = {
    Sorting.quickSort(b)(GenotypeOrdering)
//    println(b.length)
//    println("blength: "+b.length)
//    b.foreach(x => print(x.fitness+", "))
//    println

    var probs = Array.ofDim[Double](b.length)
    probs(0) = p
    for (i <- 1 until b.length) {
      probs(i) = probs(i - 1) + p * pow((1 - p), i)
    }

    var rand = new Random()
    var r = rand.nextDouble()

    var i = 0
    while (r > probs(i) && i < probs.size-1) i += 1
//    println(b(i))
    b(i)
  }


  def getTopIndividual(): Genotype = {
    topInd
  }

  object GenotypeOrdering extends Ordering[Genotype] {
    def compare(a: Genotype, b: Genotype) = b.fitness compare a.fitness
  }

}
