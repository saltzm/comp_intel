import scala.io.Source

class GASolver[T] (operators: Array[_ <: Product with Serializable with Operator],
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
      println(nGens)
      operators.foreach( o => o operate p )
      p = selector.select(p)
      // TODO: Fix so that it can work with either maximizing or minimizing
      if (selector.topFitness > maxFitness) {
        stableGens = 0
        maxFitness = selector.topFitness
      }
      println(selector.topFitness)
      stableGens += 1
      nGens += 1
      //if(nGens % 100 == 0) println(maxFitness)
    }
    selector.topInd
  }
}

object SIBSolver extends App {
  val crossoverProb = 0.7//0.7
  val mutationProb = 0.015//0.005
  val popSize = 100000
  val bracketSize = 4
  val elitism = true
  val maxGens = 100000
  val gensToConverge = 100

  val dimension = 8  
  val maxSnakeLength = 99 //math.pow(2, dimension).toInt 

  val operators = Array(OnePointCrossover(crossoverProb), 
                        TransitionMutationSnake(mutationProb, dimension))
  val fitnessFunction = new LengthFitnessFunction(dimension)
  val selector = new TournamentSelector[Int] (bracketSize, elitism, fitnessFunction)
  val populationGenerator = new TransitionPopulationGenerator(maxSnakeLength, dimension)
  val solver = new GASolver(operators, selector, populationGenerator, popSize,
                            maxGens, gensToConverge)

  val solution = solver.solve
  val solFit = fitnessFunction.buildSnake(solution)._2
  println("Snake length: " + solFit)
  println("Solution: "+solution.genes.slice(0, solFit.toInt).deep.mkString(", "))
  //println("Snake: " + fitnessFunction.buildSnake(solution.genes).deep.mkString(","))
}
