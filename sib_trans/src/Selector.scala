import scala.util.Random

trait Selector[T] { 
  def topInd: Genotype[T]
  def topFitness: Double 
  def select (population: Array[Genotype[T]]): Array[Genotype[T]] 
}

class TournamentSelector[T] (bracketSize: Int, 
                             elitism: Boolean = true, 
                             fitFunc: FitnessFunction) extends Selector[T] {
  var topIndividual: Genotype[T] = _ 
  var topIndFitness: Double = _ 

  override def topInd = topIndividual
  def topFitness: Double = topIndFitness
  // TODO:  Allow to switch between minimizing and maximizing?
  def select (pop: Array[Genotype[T]]): Array[Genotype[T]] = {
    // Must set topIndividual initially
    if (topIndividual == null || !elitism) {
      topIndividual = pop(0)
      topIndFitness = fitFunc.getFitness(topIndividual)
    }
    // Create new population to story new individuals
    var newPop = Array.ofDim[Genotype[T]](pop.length)
    // For choosing bracket members
    var rand = new Random
    // For each slot in the new population, hold a competition
    for (i <- 0 until pop.size) {
      // bracket contains bracketSize Genotypes
      val bracket = Array.ofDim[Genotype[T]] (bracketSize)
      for (b <- 0 until bracketSize) {
        // Choose a random member of the population to compete
        bracket(b) = pop(rand.nextInt(pop.size))
      }
      // Compete, and place the winner in the current slot in the new 
      // population.
      newPop(i) = compete(bracket) 
      // If the fitness of the new member is greater than the fitness of the
      // current topIndividual, we need to set the topIndividual to newPop(i)
      val newFit = fitFunc.getFitness(newPop(i))
      if (newFit > topIndFitness) {
        topIndividual = Genotype[T](newPop(i).genes.clone)
        topIndFitness = newFit 
      }
    }
    if (elitism) {
      newPop(0) = Genotype[T](topIndividual.genes.clone) 
    }
    newPop
  }

  def compete[T](bracket: Array[Genotype[T]]): Genotype[T] = {
    var best = bracket(0)
    var bestFit = fitFunc.getFitness(best)
    // Find individual with the best fitness in the bracket
    for(i <- 1 until bracket.length) {
      val curFit = fitFunc.getFitness(bracket(i))
      if (curFit > bestFit) {
        best = bracket(i)
        bestFit = curFit 
      }
    }
    Genotype[T](best.genes.clone)
  }
}
