import scala.util.{Random, Sorting}
import collection.mutable.HashMap

case class Population(members: List[MidtermGenotype]) 

object MidtermSolver extends App {
  val populationSize = 5000
  val nGenerations = 100000
  val gensToConvergence = 500
  val repetitions = args(0).toInt 
  val innerRepetitions = args(1).toInt
  val crossProb = 0.50
  val mutProb = 0.05
  val maxMutProb = 0.1
  val bracketSize = 16
  val elitism = true
  val fitFunc = new MidtermFitnessFunction 
  var members = Array.ofDim[MidtermGenotype](populationSize)
  val r = new Random()
  var selectionEngine = new TournamentSelectionEngine(bracketSize, 1, elitism) 
  
    //iterate through the generations
  var fitMap = new HashMap[Double, Int]()
  for (rep <- 0 until repetitions) {
    var maxFitness = 0.0
    for (inRep <- 0 until innerRepetitions) {
      println("inRep: "+rep)
      var currentTopFitness = 0.0
      var convergenceCounter = 0
      selectionEngine = new TournamentSelectionEngine(bracketSize, 1, elitism) 
      generatePopulation(members)
      for (gen <- 0 until nGenerations if (convergenceCounter <= gensToConvergence)) {
        //println(gen)
        var i = 0;
        //perform crossover on the population, iterating 2 at a time
        while (i < populationSize - 2) {
          //val (m1, m2) = cross (members(i), members(i + 1), crossProb)
          val (m1, m2) = adjCross2 (members(i), members(i + 1))
          members(i) = m1
          members(i + 1) = m2
          i = i + 2
          //println(i)
        }
        //perform mutation on the population
        //for ( i <- 0 until populationSize ) { members(i) = mutate (members(i), mutProb)} 
      for ( i <- 0 until populationSize ) { 
          members(i) = adaptiveMutate (members(i), convergenceCounter, 
                                       gensToConvergence, maxMutProb)
        } 
        //println("mutated")
        //select from the population
        members = selectionEngine.select(members)
        //println(selectionEngine.topInd.fitness)
        if (selectionEngine.topInd.fitness == currentTopFitness) convergenceCounter += 1 
        else {currentTopFitness = selectionEngine.topInd.fitness; convergenceCounter = 0}
      }
      if (selectionEngine.topInd.fitness > maxFitness ) 
        maxFitness = selectionEngine.topInd.fitness
    }
    fitMap.put(maxFitness, fitMap.getOrElse(maxFitness, 0) + 1)
    println("Best gene:    " + fitFunc.getX(selectionEngine.topInd.genes) + ", "+ 
                               fitFunc.getY(selectionEngine.topInd.genes))
    println("Best fitness: " + maxFitness)
  }
  val sorted = fitMap.values.toArray
  Sorting.quickSort(sorted)
  val sum = sorted.sum
  for (a <- sorted) println(a.toDouble/sum)
 
  def adjCross (g1: MidtermGenotype, g2: MidtermGenotype):
                            (MidtermGenotype, MidtermGenotype) = {
    val r = new Random()
    var crossed = false
    var prob = 1.0/math.pow(2, g1.genes.length.toDouble)
    for (i <- 0 until g1.genes.length) {
      prob = prob * 2 
      if (r.nextDouble < prob) {
        val temp = g1.genes(i)
        g1.genes(i) = g2.genes(i)
        g2.genes(i) = g1.genes(i)
        crossed = true
      }
    }
    if (crossed) (MidtermGenotype(g1.genes, fitFunc), MidtermGenotype(g2.genes, fitFunc))
    else (g1, g2)
  }
  
  def adjCross2(g1: MidtermGenotype, g2: MidtermGenotype):
                            (MidtermGenotype, MidtermGenotype) = {
    val r = new Random()
    var crossed = false
    var prob = 1.0/math.pow(2, g1.genes.length.toDouble/2.0)
    for (i <- 0 until g1.genes.length) {
      if (i == g1.genes.length/2) prob = 1.0/math.pow(2, g1.genes.length.toDouble/2.0) 
      else prob = prob * 2 
      if (r.nextDouble < prob) {
        val temp = g1.genes(i)
        g1.genes(i) = g2.genes(i)
        g2.genes(i) = g1.genes(i)
        crossed = true
      }
    }
    if (crossed) (MidtermGenotype(g1.genes, fitFunc), MidtermGenotype(g2.genes, fitFunc))
    else (g1, g2)
  }

  def cross (g1: MidtermGenotype, g2: MidtermGenotype, prob: Double): 
                                  (MidtermGenotype, MidtermGenotype) = {
    val r = new Random()
    var crossed = false //efficiency
    for (i <- 0 until g1.genes.length) {
      if (r.nextDouble < prob) {
        val temp = g1.genes(i)
        g1.genes(i) = g2.genes(i)
        g2.genes(i) = g1.genes(i)
        crossed = true
      }
    }
    if (crossed) (MidtermGenotype(g1.genes, fitFunc), MidtermGenotype(g2.genes, fitFunc))
    else (g1, g2)
  }

  def adaptiveMutate (g: MidtermGenotype,  
                      convergenceCounter: Int, 
                      gensToConvergence: Int, maxProb: Double): MidtermGenotype = {
    val r = new Random()
    val prob = convergenceCounter.toDouble/gensToConvergence.toDouble * maxProb
    //println(prob);
    var mutated = false
    for (i <- 0 until g.genes.length) {
      if (r.nextDouble < prob) {
        g.genes(i) = if (g.genes(i) == 1) 0 else 1
        mutated = true
      }
    }
    if (mutated) MidtermGenotype(g.genes, fitFunc)
    else g
  }

  def mutate (g: MidtermGenotype, prob: Double): MidtermGenotype = {
    val r = new Random()
    var mutated = false
    for (i <- 0 until g.genes.length) {  //TODO: may be an issue
      if (r.nextDouble < prob) {
        g.genes(i) = if (g.genes(i) == 1) 0 else 1 //TODO: might suck    
        mutated = true
      }
    }
    //for (a <- g.genes) print(a + " ")
    //println
    if (mutated) MidtermGenotype(g.genes, fitFunc)
    else g
  }

  def generatePopulation (members: Array[MidtermGenotype]) {
    //TODO: 44 bits
    for (i <- 0 until populationSize) {
      var x = Array.ofDim[Int](64)
      for (j <- 0 until x.length) x(j) = r.nextInt(2)
      members(i) = MidtermGenotype(x, fitFunc)
    }
  }
}
