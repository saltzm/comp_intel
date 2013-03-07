import scala.util.{Random, Sorting}
import collection.mutable.HashMap

object MidtermPSO_Solver extends App {
  val r = new Random()
  val popSize = 100000
  val nGens = 10000
  val gensToConverge = 100
  val v_max = 6.0
  val v_min = -6.0
  val repetitions = args(0).toInt

  val particles = Array.ofDim[Particle](popSize)
  var gBest = Array.ofDim[Int](64) 
  var gBestFitness = 0.0
  val fitnessFunction = new MidtermPSOFitnessFunction

  var fitMap = new HashMap[Double, Int]()
  for (rep <- 0 until repetitions){
    var genSame = 0
    generatePopulation(particles)
    gBest = particles(0).x
    gBestFitness = particles(0).fitness 
    for (gen <- 0 until nGens if genSame < gensToConverge) {
      for (p <- particles) {
        new_update(p)
        //classic_update(p)
        if (p.fitness > gBestFitness) {
          gBest = p.x.clone
          gBestFitness = p.fitness
          println(gBestFitness)
          genSame = 0
        }
      }
      genSame = genSame + 1
    }
    println("Best particle: " + fitnessFunction.getX(gBest) + ", " + 
                                fitnessFunction.getY(gBest))
    println("Best fitness: " + gBestFitness);
    fitMap.put(gBestFitness, fitMap.getOrElse(gBestFitness, 0) + 1) 
  }
  val sorted = fitMap.values.toArray
  Sorting.quickSort(sorted)
  val sum = sorted.sum
  for (a <- sorted) println(a.toDouble/sum)

  def new_update(p: Particle) {
    val c1 = 2
    val c2 = 2
    for (d <- 0 until p.x.length) {
      p.v(d) = math.min(c1 * r.nextDouble() * (p.pBest(d) - p.x(d)) + 
               c2 * r.nextDouble() * (gBest(d) - p.x(d)), v_max)
      p.v(d) = math.max(p.v(d), v_min)
      p.x(d) = math.abs(p.v(d).toInt % 2)
    }
    calculateFitness(p)
  }

  def classic_update(p: Particle){
     val c1 = 2
    val c2 = 2
    val w = 1
    for (d <- 0 until p.x.length) {
      p.v(d) = Math.min(w * p.v(d) +
               c1 * r.nextDouble() * (p.pBest(d) - p.x(d)) + 
               c2 * r.nextDouble() * (gBest(d) - p.x(d)), v_max);
      p.v(d) = math.max(p.v(d), v_min)
      val vMod = 1.0 / (1 + math.pow(math.E, (-1) * p.v(d)))
      if (r.nextDouble() < vMod) p.x(d) = 1
      else p.x(d) = 0
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
      var x = Array.ofDim[Int](64)
      for (j <- 0 until x.length) x(j) = r.nextInt(2)
      var v = Array.ofDim[Double](64)
      for (j <- 0 until v.length) v(j) = r.nextDouble() * v_max * negOrPos
      particles(i) = Particle(x, v)
      fitnessFunction.calculateFitness(particles(i))
    }
  }

  def negOrPos : Double = math.pow(-1, r.nextInt(2))
}
