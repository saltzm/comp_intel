import scala.util.{Random, Sorting}
import collection.mutable.HashMap

case class Population(members: List[MSEGenotype]) 

object Exhaustive extends App {
  var max = 0.0
  val fitFunc = new MSEFitnessFunction(672, 1495)
  val maxes = Array[Int] (42, 9, 168, 56, 7, 92, 4)
  for (a <- 0 until 42){
    for (b <- 0 until 9){
      for (c <- 0 until 168){
        for (d <- 0 until 56) {
          for (e <- 0 until 7) {
            for (f <- 0 until 92) {
              for (g <- 0 until 4) {
                val current = MSEGenotype(Array(a, b, c, d, e, f, g), fitFunc)
                if (current.fitness > max) {
                  println(max)
                  max = MSEGenotype(Array(a, b, c, d, e, f, g), fitFunc).fitness 
                }
              }
            }
          }
        }
      }
    }
  }
}

object MSESolver extends App {
  val populationSize = 1000
  val nGenerations = 3500
  val gensToConvergence = 50
  val repetitions = args(0).toInt 
  val innerRepetitions = args(1).toInt
  val crossProb = 0.5 
  val mutProb = 0.15
  val bracketSize = 10
  val elitism = true
  //val msrt = 672
  val msrt = args(2).toInt
  //val dnvt = 1495
  val dnvt = args(3).toInt
  val fitFunc = new MSEFitnessFunction(msrt, dnvt)
  val maxes = Array[Int] (42, 9, 168, 56, 7, 92, 4)
  var members = Array.ofDim[MSEGenotype](populationSize)
  val r = new Random()

    //iterate through the generations
  var fitMap = new HashMap[Double, Int]()
  for (rep <- 0 until repetitions) {
    var maxFitness = 0.0
    for (inRep <- 0 until innerRepetitions) {
      println(rep+", "+inRep)
      var currentTopFitness = 0.0
      var convergenceCounter = 0.0
      val selectionEngine = new TournamentSelectionEngine(bracketSize, 1, elitism) 
      generatePopulation(members)
      for (gen <- 0 until nGenerations if (convergenceCounter <= gensToConvergence)) {
        //println(gen)
        var i = 0;
        //perform crossover on the population, iterating 2 at a time
        while (i < populationSize - 2) {
          val (m1, m2) = cross (members(i), members(i + 1), crossProb)
          members(i) = m1
          members(i + 1) = m2
          i = i + 2
          //println(i)
        }
        //perform mutation on the population
        for ( i <- 0 until populationSize ) { members(i) = mutate (members(i), mutProb)} 
        //println("mutated")
        //select from the population
        members = selectionEngine.select(members)
        //println(selectionEngine.topInd.fitness)
        if (selectionEngine.topInd.fitness == currentTopFitness) convergenceCounter += 1 
        else {currentTopFitness = selectionEngine.topInd.fitness; convergenceCounter = 0}
        //println(selectionEngine.topInd.fitness)
      }
      if (selectionEngine.topInd.fitness > maxFitness ) {
        maxFitness = selectionEngine.topInd.fitness 
        println(selectionEngine.topInd.genes.deep.mkString(", "))
      }
    }
    fitMap.put(maxFitness, fitMap.getOrElse(maxFitness, 0) + 1)
    println(maxFitness)
  }
  println("-----------------------")
  val sorted = fitMap.values.toArray
  Sorting.quickSort(sorted)
  val sum = sorted.sum
  for (a <- sorted) println(a.toDouble/sum)
  
  def cross (g1: MSEGenotype, g2: MSEGenotype, prob: Double): 
                                  (MSEGenotype, MSEGenotype) = {
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
    if (crossed) (MSEGenotype(g1.genes, fitFunc), MSEGenotype(g2.genes, fitFunc))
    else (g1, g2)
  }

  def av_cross (g1: MSEGenotype, g2: MSEGenotype, prob: Double): 
                                  (MSEGenotype, MSEGenotype) = {
    val r = new Random()
    var crossed = false //efficiency
    for (i <- 0 until g1.genes.length) {
      if (r.nextDouble < prob) {
        val av = (g1.genes(i) + g1.genes(i))/2
        g1.genes(i) = av 
        g2.genes(i) = av 
        crossed = true
      }
    }
    if (crossed) (MSEGenotype(g1.genes, fitFunc), MSEGenotype(g2.genes, fitFunc))
    else (g1, g2)
  }



  def mutate (g: MSEGenotype, prob: Double): MSEGenotype = {
    val r = new Random()
    var mutated = false
    for (i <- 0 until g.genes.length) {  //TODO: may be an issue
      if (r.nextDouble < prob) {
        g.genes(i) = r.nextInt(maxes(i) + 1) //TODO: might suck    
        //g.genes(i) = (g.genes(i) + r.nextInt(10)) % maxes(i)
        //g.genes(i) = math.min(math.max(g.genes(i) + r.nextInt(maxes(i)/2)*math.pow(-1, r.nextInt(2)).toInt, 0), maxes(i))
        //g.genes(i) = math.min(math.max(g.genes(i) + r.nextInt(10)*math.pow(-1, r.nextInt(2)).toInt, 0), maxes(i))
        mutated = true
      }
    }
    //for (a <- g.genes) print(a + " ")
    //println
    if (mutated) MSEGenotype(g.genes, fitFunc)
    else g
  }

  def generatePopulation (members: Array[MSEGenotype]) {
    //generate the population
    for (i <- 0 until populationSize) {
      members(i) = MSEGenotype( 
                     Array[Int](r.nextInt(maxes(0) + 1),
                            r.nextInt(maxes(1) + 1), 
                            r.nextInt(maxes(2) + 1),
                            r.nextInt(maxes(3) + 1),                             
                            r.nextInt(maxes(4) + 1),
                            r.nextInt(maxes(5) + 1),
                            r.nextInt(maxes(6) + 1) ), fitFunc)
    }
  }
}
