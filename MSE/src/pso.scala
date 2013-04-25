import scala.util.{Random, Sorting}
import collection.mutable.HashMap

case class Particle (x: Array[Int], v: Array[Double]) {
  var pBest = x.clone 
  var pBestFitness = 0.0
  var fitness = 0.0
  def nc = x(0)
  def len = x(1)
  def s1 = x(2)
  def s2 = x(3)
  def scc = x(4)
  def rau = x(5)
  def nai = x(6)
}


object PSO_Solver extends App {
  val r = new Random()
  val popSize = 100
  val nGens = 100000
  val gensToConverge = 15000
  val v_max = 2.0
  val v_min = -2.0
  val repetitions = args(0).toInt
  val inReps = args(1).toInt

  val particles = Array.ofDim[Particle](popSize)
  val maxes = Array[Int] (42, 9, 168, 56, 7, 92, 4)
  val mins = Array[Int] (1, 0, 0, 0, 1, 0, 0)
  var gBest = Array(0, 0, 0, 0, 0, 0, 0)
  var gBestFitness = 0.0
  val fitnessFunction = new MSEPSOFitnessFunction(args(2).toInt, args(3).toInt)

  var fitMap = new HashMap[Double, Int]()
  for (rep <- 0 until repetitions){
    var maxGBest = 0.0
    var maxGBInd = Particle(Array(), Array())
    for (inRep <- 0 until inReps) {
      println(rep + ", " + inRep)
      var genSame = 0
      gBest = Array(0, 0, 0, 0, 0, 0, 0)
      gBestFitness = 0.0
      generatePopulation(particles)
      for (gen <- 0 until nGens if genSame < gensToConverge) {
        for (p <- particles) {
          //new_update(p)
          classic_update(p)
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
    println("Best fitness: " + maxGBest);
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
      p.x(d) = math.max(mins(d), p.v(d).toInt % maxes(d))
    }
    calculateFitness(p)
  }

  def classic_update(p: Particle){
     val c1 = 2
    val c2 = 2
    val w = 1
    for (d <- 0 until p.x.length) {
      p.v(d) = w * p.v(d) +
               c1 * r.nextDouble() * (p.pBest(d) - p.x(d)) + 
               c2 * r.nextDouble() * (gBest(d) - p.x(d))
      p.x(d) = math.max(mins(d), math.min(p.x(d) + p.v(d).toInt, maxes(d)))
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
      particles(i) = Particle( Array[Int] (r.nextInt(maxes(0) + 1),
                                           r.nextInt(maxes(1) + 1),
                                           r.nextInt(maxes(2) + 1),          
                                           r.nextInt(maxes(3) + 1), 
                                           r.nextInt(maxes(4) + 1),
                                           r.nextInt(maxes(5) + 1),
                                           r.nextInt(maxes(6) + 1)),
                            Array[Double] (r.nextDouble() * v_max * negOrPos, 
                                           r.nextDouble() * v_max * negOrPos,
                                           r.nextDouble() * v_max * negOrPos,
                                           r.nextDouble() * v_max * negOrPos, 
                                           r.nextDouble() * v_max * negOrPos,
                                           r.nextDouble() * v_max * negOrPos,
                                           r.nextDouble() * v_max * negOrPos)) 
    }
  }

  def negOrPos : Double = math.pow(-1, r.nextInt(2))
}
