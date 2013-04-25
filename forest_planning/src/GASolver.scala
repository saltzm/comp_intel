import scala.io.Source

class GASolver[T] (operators: Array[Product with Serializable with Operator],
                selector: Selector[T],
                populationGenerator: PopulationGenerator,
                popSize: Int,
                maxGens: Int,
                gensToConverge: Int) {
  def solve: Genotype[T] = {
    var p = populationGenerator.generate[T](popSize)
    var maxFitness = Double.MinValue  //TODO:  assumes maximizing 
    var stableGens = 0
    var nGens = 0
    while (nGens < maxGens && stableGens < gensToConverge) {
      operators.foreach( o => o operate p )
      p = selector.select(p)
      // TODO: Fix so that it can work with either maximizing or minimizing
      if (selector.topFitness > maxFitness) {
        stableGens = 0
        maxFitness = selector.topFitness
      }
      stableGens += 1
      nGens += 1
      if(nGens % 100 == 0) println(maxFitness)
    }
    selector.topInd
  }
}

object GetPeriods extends App {
  val genes = Array.ofDim[Int](73)
  val lines = Source.fromFile(args(0)).getLines
  var i = 0
  for (l <- lines) {
    genes(i) = l.trim.toInt
    i += 1
  }
  val fitnessFunction = new ForestPlanningFitnessFunction("West_73_units_adjacency.txt",
                                                          "West_73_units_volumes.txt",
                                                          73,
                                                          3,
                                                          34467)
  val solution = Genotype[Int](genes)
  println("Error: " + (-1*fitnessFunction.getFitness(solution)))
  println("Harvest per Period: ")
  for (i <- 1 to 3){
    println("Period "+i+": "+fitnessFunction.getPeriodHarvests(solution)(i-1))
  }
  println("Number of Adjacency Violations: " + fitnessFunction.countViolations(solution.genes))
}

object ForestPlanningSolver extends App {
  val crossoverProbs = Set[Double](0.7) //Set[Double](0.3, 0.5, 0.7, 0.9)
  val mutationProbs = Set[Double](0.015)//Set[Double](0.005, 0.015, 0.1, 0.3)
  val popSize = 1000
  val bracketSize = 4
  val elitism = true
  val maxGens = 100000
  val gensToConverge = 1000
  val nTimePeriods = 3
  val nStands = 73 
  val target = 34467 
  val adjacencyFile = "West_73_units_adjacency.txt"
  val volumeFile = "West_73_units_volumes.txt" 
 
  for (crossoverProb <- crossoverProbs) {
    for (mutationProb <- mutationProbs) {
      //val operators = Array(TwoPointCrossover(crossoverProb), 
      val operators = Array(TwoPointCrossover(crossoverProb), 
                            SimpleMutationForestPlanning(mutationProb))
      val fitnessFunction = new ForestPlanningFitnessFunction(adjacencyFile,
                                                              volumeFile,
                                                              nStands,
                                                              nTimePeriods,
                                                              target)
      val selector = new TournamentSelector[Int] (bracketSize, elitism, fitnessFunction)
      val populationGenerator = new ForestPlanningPopulationGenerator(nTimePeriods, nStands)
      val solver = new GASolver(operators, selector, populationGenerator, popSize,
                                maxGens, gensToConverge)

      val solution = solver.solve
      println("cross prob: "+ crossoverProb)
      println("mut prob: " + mutationProb)
      println("Error: " + (-1*fitnessFunction.getFitness(solution)))
      println("Harvest per Period: ")
      for (i <- 1 to nTimePeriods){
        println("Period "+i+": "+fitnessFunction.getPeriodHarvests(solution)(i-1))
      }
      println("Number of Adjacency Violations: " 
        + fitnessFunction.countViolations(solution.genes))
      println("Solution: "+solution.genes.deep.mkString(","))
    }
  }
}
