import scala.util.{Random, Sorting}
import collection.mutable.HashMap

case class Particle (x: Array[Int], v: Array[Double]) {
  var pBest = x.clone 
  var pBestFitness = Double.MinValue 
  var fitness = Double.MinValue 
}


object PSO_Solver extends App {
  val r = new Random()
  val popSize = 10000
  val nGens = 100000
  val gensToConverge = 1000
  val v_max = 2.0
  val v_min = -2.0
  val repetitions = args(0).toInt
  val inReps = args(1).toInt

  val particles = Array.ofDim[Particle](popSize)
  val maxes = Array.ofDim[Int](73).map(x => 3)
  var gBest = Array.ofDim[Int](73) 
  var gBestFitness = Double.MinValue
  val fitnessFunction =  new PSOForestPlanningFitnessFunction("West_73_units_adjacency.txt",
                                                          "West_73_units_volumes.txt",
                                                          73,
                                                          3,
                                                          34467)


  var fitMap = new HashMap[Double, Int]()
    for (rep <- 0 until repetitions){
      var maxGBest = Double.MinValue 
      var maxGBInd = Particle(Array(), Array())
      for (inRep <- 0 until inReps) {
        //println(rep + ", " + inRep)
        var genSame = 0
        gBest = Array.ofDim[Int](73)
        gBestFitness = Double.MinValue 
        generatePopulation(particles)
        for (gen <- 0 until nGens if genSame < gensToConverge) {
          for (p <- particles) {
            //new_update(p)
            classic_update(p)
            //adaptive_update(p, gen, 7)
            //adaptive_update2(p, gen, 7)
            if (p.fitness > gBestFitness) {
              gBest = p.x.clone
              gBestFitness = p.fitness
              //println(gBestFitness)
              genSame = 0
            }
          }
          //println(gBestFitness)
          genSame = genSame + 1
        }
        if (gBestFitness > maxGBest) { 
          maxGBest = gBestFitness
          maxGBInd = Particle(gBest, Array())
        }
      }
      println("Best particle: " + maxGBInd.x.deep.mkString(", "))
      println("Violations: " + fitnessFunction.countViolations(maxGBInd.x))
      println("Best fitness: " + maxGBest)
      fitMap.put(maxGBest, fitMap.getOrElse(maxGBest, 0) + 1) 
    }

  val sorted = fitMap.values.toArray
  Sorting.quickSort(sorted)
  val sum = sorted.sum
  for (a <- sorted) println(a.toDouble/sum)

  def new_update(p: Particle) {
    val c1 = 3
    val c2 = 2
    for (d <- 0 until p.x.length) {
      p.v(d) = c1 * r.nextDouble() * (p.pBest(d) - p.x(d)) + 
               c2 * r.nextDouble() * (gBest(d) - p.x(d))
      p.x(d) = math.max(0, math.abs(p.v(d).toInt) % (maxes(d)+1))
    }
    //println(p.x.deep.mkString(", "))
    calculateFitness(p)
  }

  def classic_update(p: Particle){
     val c1 = 3 
    val c2 = 1
    val w = 0.1
    for (d <- 0 until p.x.length) {
      p.v(d) = w * p.v(d) +
               c1 * r.nextDouble() * (p.pBest(d) - p.x(d)) + 
               c2 * r.nextDouble() * (gBest(d) - p.x(d))
      p.x(d) = math.max(0, math.min(p.x(d) + p.v(d).toInt, maxes(d)))
    }
    calculateFitness(p)
  }

  def adaptive_update(p: Particle, gen: Int, divisor: Int) {
    val c1 = 3 
    val c2 = 1
    val w = math.abs(math.sin(gen)/divisor) 
    for (d <- 0 until p.x.length) {
      p.v(d) = w * p.v(d) +
               c1 * r.nextDouble() * (p.pBest(d) - p.x(d)) + 
               c2 * r.nextDouble() * (gBest(d) - p.x(d))
      p.x(d) = math.max(0, math.min(p.x(d) + p.v(d).toInt, maxes(d)))
    }
    calculateFitness(p)
  }

  def adaptive_update2(p: Particle, gen: Int, divisor: Int) {
    val c1 = 3 
    val c2 = 1
    val w = math.sin(gen)/(2*divisor) + 1.0/divisor 
    for (d <- 0 until p.x.length) {
      p.v(d) = w * p.v(d) +
               c1 * r.nextDouble() * (p.pBest(d) - p.x(d)) + 
               c2 * r.nextDouble() * (gBest(d) - p.x(d))
      p.x(d) = math.max(0, math.min(p.x(d) + p.v(d).toInt, maxes(d)))
    }
    calculateFitness(p)
  }

  def calculateFitness(p: Particle) {
    p.fitness = fitnessFunction.calculateFitness(p)   
    if (p.fitness > p.pBestFitness){
      p.pBest = p.x.clone
      p.pBestFitness = p.fitness
    }
  }

  def generatePopulation(particles: Array[Particle]) {
    for (i <- 0 until particles.length) {
      particles(i) = Particle( 
        Array.ofDim[Int](73).map(x => r.nextInt(4)),
        Array.ofDim[Double](73).map(x => r.nextDouble() * v_max * negOrPos)
      ) 
    }
  }

  def negOrPos : Double = math.pow(-1, r.nextInt(2))
}
